knitr::opts_chunk$set(echo = TRUE)
# Determine output format (html, pdf, docx) from RMarkdown
output <- knitr::opts_knit$get("rmarkdown.pandoc.to")
if (knitr::is_latex_output()){output<-"pdf"}
if (is.null(output)) {output <- "html"}
# Determine output type (HTML or non-HTML)
is_html_output <- output %in% c("html", "html4", "html5")
is_interactive <- interactive()

# Installation and loading of libraries
options(kableExtra.auto_format = FALSE)
suppressWarnings(lapply(c("kableExtra", "knitr","flextable"), function(pkg) {
  if (!require(pkg, character.only = TRUE))
    install.packages(pkg, repos = "https://mirrors.nic.cz/R/")
  library(pkg, character.only = TRUE)
}))
# Custom function for table creation
default_kable <- function(data, ...) {
  if(output == "docx") {
  flextable(data) %>% 
      set_table_properties(width = 1, layout = "autofit") %>%
      fontsize(size = 7, part = "body") %>% fontsize(size = 8, part = "header") %>%
      height(height = 0.5, part = "body") 
  } else if (output == "pdf") {
    # Output for PDF (LaTeX)
    kable(data, format = "latex", booktabs = TRUE, position = "H", ...) %>%
    kable_styling(font_size = 7)
  } else if (output == "html") {
    kable(data, format = "html", booktabs = TRUE, ...) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                    font_size = 7, full_width = FALSE, position = "center") %>%
      knitr::asis_output()
  } else {
    # Safety fallback to LaTeX
    kable(data, booktabs = TRUE, ...) %>%
    kable_styling(font_size = 7) %>%
    print()
  }
}

# List of required libraries
required_packages <- c("terra", "ggplot2", "dplyr", "tidyr", "viridis", "sf", "easyclimate", "curl", "webshot2", "chromote")

# Installation and loading of libraries
suppressWarnings(
  lapply(required_packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg, repos = "https://mirrors.nic.cz/R/")
    library(pkg, character.only = TRUE)
  })
)

# Clean up variables in R environment
rm(required_packages)

# Create folder for downloaded data if it doesn't exist
dir.create("./data/rastry", recursive = TRUE, showWarnings = FALSE)

# URL for file download (file T.air_15_cm.mean.tif from Zenodo archive, size 97.0 MB)
soubor <- c("T.air_15_cm.mean.tif")

url <- paste0("https://zenodo.org/record/6352641/files/",soubor)

# Path where to save file
destfile <- paste0("./data/rastry/", soubor)

# Create folder if it doesn't exist
dir.create("./data/rastry", recursive = TRUE, showWarnings = FALSE)

# download raster using curl
curl::multi_download(url, destfile, resume = T)

# Loading raster using terra library
zeta <- terra::rast(destfile)

# Setting space for legend
par(mar = c(5, 4, 4, 5))  # Adding space on right side for legend
par(oma = c(2, 1, 1, 1))  # Increasing outer bottom margin

# Basic plot of raster using plot() function
plot(zeta, col = viridis::viridis(100), 
     xlab = "Longitude", ylab = "Latitude")

# Adding label to legend with unit (°C)
mtext("Temperature (°C)", side = 4, line = 3)  # Adding label on right side

# Calculate average value of raster
values_zeta <- values(zeta)
mean_zeta <- mean(values_zeta, na.rm = TRUE)

# Create histogram for raster values
hist_zeta <- hist(values_zeta, main = "Temperature Histogram", 
                  xlab = "Average Annual Temperature (°C)", ylab = "Frequency",
                  col = "lightblue", breaks = 20, 
                  probability = TRUE, border = "gray")

# Add vertical line indicating average
abline(v = mean_zeta, col = "red", lwd = 2)

# Add label with average value
text(mean_zeta, par("usr")[4] * 0.92,
     labels = paste0("Average: ", round(mean_zeta, 2)," °C"),
     col = "red", pos = 4)

# Define center coordinates in WGS84 (EPSG:4326)
lat <- 48.9712522 # Březnická Lodge
lon <- 13.4825903
coords <- data.frame(lon, lat)

# Create sf object with coordinates in EPSG:4326
point_wgs84 <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)

# Transform point to coordinate system matching raster data
# (3-degree Gauss-Kruger zone 4, EPSG:31468)
point_epsg31468 <- st_transform(point_wgs84, 31468)

# Get coordinates for creating rectangle (bounding box) around point
point_coords <- st_coordinates(point_epsg31468)
x_center <- point_coords[1, "X"]
y_center <- point_coords[1, "Y"]

# Define subset size (e.g., 500x500 meters around point)
buffer_size <- 500  # half size (in meters)
xmin <- x_center - buffer_size
xmax <- x_center + buffer_size
ymin <- y_center - buffer_size
ymax <- y_center + buffer_size

# Create rectangle (bbox)
bbox <- terra::ext(xmin, xmax, ymin, ymax)

# Crop raster using rectangle
zeta_cropped <- crop(zeta, bbox)

# Set margins on map edges for legend
par(mar = c(4, 4, 4, 5))  # Add space on right side for legend
par(oma = c(2, 1, 1, 1))  # Increase outer bottom margin

# Basic plot of raster using plot() function
plot(zeta_cropped, col = viridis::viridis(100), 
     xlab = "Longitude (m), EPSG:31468", 
     ylab = "Latitude (m)")

# Add label to legend with unit (°C)
mtext("Temperature (°C)", side = 4, line = 3)  # Add label on right side

# Add point with coordinates on map ('x' symbol)
points(x_center, y_center, pch = 4, col = "red", cex = 1.5)  # pch=4 is 'x' symbol

# Clean up unnecessary variables
rm(url,point_epsg31468, bbox, coords, lat, lon, x_center, xmax, 
   xmin, y_center, ymin, ymax, point_coords, buffer_size)

# Loading lat/lon coordinates from CSV that contains 'lat' and 'lon' columns
coords_data <- read.csv("data/rastry/souradnice.csv", sep=";")

# Creating sf object from coordinates in EPSG:4326 (WGS84)
points_wgs84 <- st_as_sf(coords_data, coords = c("lon", "lat"), crs = 4326)

# Transforming points to EPSG:31468
points_epsg31468 <- st_transform(points_wgs84, 31468)

# Converting sf object to format suitable for terra::extract
coords_matrix <- st_coordinates(points_epsg31468)

# Extracting values from raster for coordinates using bilinear interpolation
values <- terra::extract(zeta, coords_matrix, method = "bilinear")

# Joining extracted values with coordinates and saving to new table
final_data <- cbind(coords_data, values)
default_kable(final_data)

# Setting space for legend and plot
par(mar = c(4, 4, 4, 5))  # Adding space on right side for legend
par(oma = c(2, 1, 1, 1))  # Increasing outer bottom margin

# Plotting raster - for checking that all points are inside map
plot(zeta, main = "Visualization of raster with coordinates", 
     col = viridis(100),  # Using viridis color scale
     xlab = "Longitude (m), EPSG:31468", 
     ylab = "Latitude (m)")

# Adding legend with unit (°C)
mtext("Temperature (°C)", side = 4, line = 3)

# Adding all points from coordinates as crosses (pch=4 is 'x' symbol)
points(coords_matrix[,1], coords_matrix[,2], pch = 4, col = "red", cex = 1.5)  

# Clean up unnecessary variables
rm(coords_data, points_wgs84, points_epsg31468, coords_matrix, values)

# Loading ForestClim raster using terra
forest_raster <- terra::rast("./data/rastry/ForestClim_01_CZE_NPS.tif")

# Displaying new raster
par(mar = c(5, 4, 4, 5))  # Setting margins
plot(forest_raster, main = paste0("Visualization of raster ", "ForestClim_01"), 
     col = viridis::viridis(100), 
     xlab = "Longitude", ylab = "Latitude")

# Adding label to legend
mtext("Temperature (°C)", side = 4, line = 3)  # Adding label on right side

# Histogram for ForestClim raster
forest_mean_value <- mean(values(forest_raster), na.rm = TRUE)

# Histogram for ForestClim
hist_forest <- hist(values(forest_raster), main = "ForestClim Histogram", 
                    xlab = "Mean Annual Temperature (°C)", ylab = "Count",
                    col = "lightgreen", breaks = 20, 
                    probability = TRUE, border = "gray")

# Adding vertical line indicating average for ForestClim raster
abline(v = forest_mean_value, col = "darkred", lwd = 2)

# Adding label for mean value
text(forest_mean_value, par("usr")[4] * 0.9, labels = 
     paste0("Mean: ", round(forest_mean_value, 2),"°C"), 
     col = "darkred", pos = 4)

# Using same point_wgs84, defined in previous example (Březnická Lodge)
# Transform point to EPSG:3035 (European Lambert Conformal Conic)
point_epsg3035 <- st_transform(point_wgs84, 3035)

# Get coordinates for creating rectangle (bounding box) around point
point_coords <- st_coordinates(point_epsg3035)
x_center <- point_coords[1, "X"]
y_center <- point_coords[1, "Y"]

# Define subset size (e.g., 500 meters around point)
buffer_size <- 500  
xmin <- x_center - buffer_size
xmax <- x_center + buffer_size
ymin <- y_center - buffer_size
ymax <- y_center + buffer_size

# Create rectangle (bbox)
bbox <- terra::ext(xmin, xmax, ymin, ymax)

# Crop raster using rectangle (for ForestClim raster)
forest_raster_cropped <- crop(forest_raster, bbox)

# Setting space for legend
par(mar = c(4, 4, 4, 5))  # Adding space on right side for legend
par(oma = c(2, 1, 1, 1))  # Increasing outer bottom margin

# Basic plot of raster using plot() function
plot(forest_raster_cropped, main = paste0("Visualization of ForestClim raster subset"),
     col = viridis::viridis(100),
     xlab = "Longitude (m), EPSG:3035", 
     ylab = "Latitude (m)")

# Adding label to legend
mtext("Raster values", side = 4, line = 3)  # Label on right side

# Adding point with coordinates on map ('x' symbol)
points(x_center, y_center, pch = 4, col = "red", cex = 1.5)  # pch=4 is 'x' symbol

# Values for both rasters
values_forest <- values(forest_raster)

# Calculation of common histogram breaks
breaks <- seq(min(c(values_zeta, values_forest), na.rm = TRUE), 
              max(c(values_zeta, values_forest), na.rm = TRUE), length.out = 30)

# Plotting first histogram for T.air_15_cm.mean.tif (in memory from previous calculations)
plot(hist_zeta, col = rgb(0, 0, 1, 0.5), xlim = range(breaks), 
     ylim = c(0, max(hist_zeta$counts)), 
     main = "Combined histogram of rasters with dual Y axis", 
     xlab = "Value", ylab = soubor, 
     cex.main = 1.2)  # Reducing title size

# Adding line and label for T.air_15_cm.mean.tif mean (average above line)
abline(v = mean_zeta, col = "blue", lwd = 2)
text(mean_zeta, max(hist_zeta$counts) * 0.75, labels = 
       paste0("Mean: ", round(mean_zeta, 2),"°C"), col = "blue", pos = 4)

# Plotting second y axis on right side
par(new = TRUE)
plot(hist_forest, col = rgb(0, 1, 0, 0.5), xlim = range(breaks), 
     ylim = c(0, max(hist_forest$counts)), 
     axes = FALSE, xlab = "", ylab = "", main="")
axis(side = 4)
mtext("ForestClim Frequency", side = 4, line = 3)

# Adding line and label for ForestClim mean (average below line)
abline(v = forest_mean_value, col = "darkgreen", lwd = 2)
text(forest_mean_value, max(hist_forest$counts) * 0.5, labels = 
       paste0("Mean: ", round(forest_mean_value, 2),"°C"), col = "darkgreen", pos = 4)

# Adding legend
legend("topright", legend = c("T.air_15_cm.mean", "ForestClim 01"), 
       fill = c(rgb(0, 0, 1, 0.5), rgb(0, 1, 0, 0.5)))

# Delete downloaded file
unlink(destfile)
# Clean up unnecessary variables
rm(hist_zeta, hist_forest, zeta, zeta_cropped, values_zeta, values_forest, 
   breaks, mean_zeta, forest_mean_value, bbox, forest_raster, 
   forest_raster_cropped, point_coords, point_epsg3035, point_wgs84,
   buffer_size, destfile, soubor, x_center, xmax, xmin, y_center, ymax, ymin)

# List of required libraries
required_packages <- c("terra", "ggplot2", "easyclimate", "tidyterra", 
                       "raster", "leaflet", "dplyr", "webshot2", "htmlwidgets")

# Installation and loading of libraries
suppressWarnings(
  lapply(required_packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg, repos = "https://mirrors.nic.cz/R/")
    library(pkg, character.only = TRUE)
  })
)
# Clean up variables in R environment
rm(required_packages)

## Loading shapefile from specified path (EPSG:32633)
nps_region <- vect("./data/rastry/NPS_UTM33N.shp")

## Converting shapefile to required coordinate system (EPSG:4326 for lon lat coordinates)
nps_region <- project(nps_region, "EPSG:4326")

## Downloading Tmax values for Šumava NP area between May 1-3, 2020
nps_temp <- get_daily_climate(
  coords = nps_region,
  climatic_var = "Tmax", # can be changed to "Prcp" for precipitation or "Tmin" for min temperatures
  # mean temperature is not available here, but is commonly calculated as (Tmax+Tmin)/2
  period = "2020-05-01:2020-05-03",
  output = "raster"
)

# Plotting individual layers and adding nps_region polygon boundaries
ggplot() +
  geom_spatraster(data = nps_temp) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_distiller(palette = "RdYlBu", na.value = "transparent") +
  geom_spatvector(data = nps_region, fill = NA) +
  labs(fill = "Maximum\ntemperature \n(ºC)") +
  scale_x_continuous(breaks = seq(13.3, 13.9, by = 0.3)) +
  scale_y_continuous(breaks = seq(48.8, 49.2, by = 0.2)) +
  theme_minimal()

## Downloading Tmax values for NPS area between May 1-3, 1950
nps_temp_1950 <- get_daily_climate(
  coords = nps_region,
  climatic_var = "Tmax",
  period = "1950-05-01:1950-05-03",
  output = "raster"
)

# Plotting individual layers (days) and adding nps_region polygon boundaries
ggplot() +
  geom_spatraster(data = nps_temp_1950) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_distiller(palette = "RdYlBu", na.value = "transparent") +
  geom_spatvector(data = nps_region, fill = NA) +
  labs(fill = "Maximum\ntemperature \n(ºC)") +
  scale_x_continuous(breaks = seq(13.3, 13.9, by = 0.3)) +
  scale_y_continuous(breaks = seq(48.8, 49.2, by = 0.2)) +
  theme_minimal()

# Calculate difference between temperatures (nps_temp - nps_temp_1950)
nps_temp_diff <- nps_temp - nps_temp_1950

ggplot() +
  geom_spatraster(data = nps_temp_diff) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_distiller(palette = "RdYlBu", na.value = "transparent") +
  geom_spatvector(data = nps_region, fill = NA) +
  labs(fill = "Difference \nmaximum \ntemperature \n2020-1950 \n(ºC)") +
  scale_x_continuous(breaks = seq(13.3, 13.9, by = 0.3)) +
  scale_y_continuous(breaks = seq(48.8, 49.2, by = 0.2)) +
  theme_minimal()

if (is_html_output || is_interactive) { # Code for HTML version or interactive mode
  nps_temp_diff_1 <- nps_temp_diff[[1]]  # Select layer as needed
  nps_temp_diff_1_raster <- as(nps_temp_diff_1, "Raster")
  raster_values <- raster::values(nps_temp_diff_1_raster)
  
  # Color palette for visualization
  pal <- colorNumeric(palette = "RdYlBu", domain = raster_values, 
                      na.color = "transparent")
  
  # Creating interactive map
  leaflet() %>%
    addTiles() %>%
    addRasterImage(nps_temp_diff_1_raster, colors = pal, opacity = 0.8) %>%
    addPolygons(data = as(nps_region, "Spatial"), fill = NA, color = "black") %>%
    addLegend(pal = pal, values = raster_values, title = "Difference Tmax 2020-1950 (ºC)")
  
} else { # Code for non-HTML version (PDF/Word - static image)
  nps_temp_diff_1 <- nps_temp_diff[[1]]  # Select layer as needed
  nps_temp_diff_1_raster <- as(nps_temp_diff_1, "Raster")
  raster_values <- raster::values(nps_temp_diff_1_raster)
  
  # Color palette
  pal <- colorNumeric(palette = "RdYlBu", domain = raster_values, 
                      na.color = "transparent")
  
  # Creating interactive map that we'll save as HTML
  map <- leaflet() %>%
    addTiles() %>%
    addRasterImage(nps_temp_diff_1_raster, colors = pal, opacity = 0.8) %>%
    addPolygons(data = as(nps_region, "Spatial"), fill = NA, color = "black") %>%
    addLegend(pal = pal, values = raster_values, title = "Difference Tmax 2020-1950 (ºC)")
  
  # Saving widget as HTML file
  htmlwidgets::saveWidget(map, "./images/map.html", selfcontained = TRUE)
  
  # Creating static snapshot of map using webshot
  #webshot("./images/map.html", file = "./images/map.png", cliprect = "viewport")
  webshot2::webshot("./images/map.html", file = "./images/map.png", cliprect = "viewport", 
                   vwidth = 1000, vheight = 800, delay = 5)

  
  # Inserting image into document
  knitr::include_graphics("./images/map.png", dpi=150)
}

# Clean up variables in R environment
rm(nps_temp_diff, nps_region, nps_temp_1950, nps_temp)

# URL for file download (file T.air_200_cm.mean.tif from Zenodo)
soubor<-"2020_T_mean_air_15cm_epsg32633.tif"
url <- paste0("https://gitlab.ibot.cas.cz/",
              "matej.man/microclimate-atlas-public/-/",
              "raw/main/geodata/EPSG32633/",soubor)

# Path where to save file
destfile <- paste0("./data/rastry/", soubor)

# Create folder if it doesn't exist
dir.create("./data/rastry", recursive = TRUE, showWarnings = FALSE)

# Download file if it doesn't exist
if (!file.exists(destfile)) {
  download.file(url, destfile, mode = "wb")
}

# Loading raster using terra library
npcs <- terra::rast(destfile)

# Setting space for legend
par(mar = c(5, 4, 4, 5))  # Adding space on right side for legend
par(oma = c(2, 1, 1, 1))  # Increasing outer bottom margin

# Basic plot of raster using plot() function
plot(npcs, col = viridis::viridis(100), 
     xlab = "Longitude", ylab = "Latitude")

# Adding label to legend with unit (°C)
mtext("Temperature (°C)", side = 4, line = 3)  # Adding label on right side

# Delete downloaded file
unlink(destfile)
# Clean up unnecessary variables
rm(url, npcs, soubor, destfile)
