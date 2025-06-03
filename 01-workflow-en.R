# Determine output format (html, pdf, docx) from RMarkdown
output <- knitr::opts_knit$get("rmarkdown.pandoc.to")
if (knitr::is_latex_output()){output<-"pdf"}
if (is.null(output)) {output <- "html"}

# Install and load libraries
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

# Install and load libraries
suppressWarnings(lapply("openxlsx", function(pkg) {
  if (!require(pkg, character.only = TRUE))
    install.packages(pkg, repos = "https://mirrors.nic.cz/R/")
  library(pkg, character.only = TRUE)
}))

# Loading field notes from Excel
zaznamy <- read.xlsx("./data/zaznamyR.xlsx")

default_kable(head(zaznamy, 20))

# List of required libraries
required_packages <-
  c("myClim", "dplyr", "tidyr", "stringr", "openxlsx", "ggplot2")

# Install and load libraries
suppressWarnings(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE))
    install.packages(pkg, repos = "https://mirrors.nic.cz/R/")
  library(pkg, character.only = TRUE)
}))

# Clean up variables in R environment
rm(required_packages)

## assembly of files_table from the above loaded excel table "zaznamy"
## in combination with paths to downloaded files from loggers
## Getting paths to downloaded files
soubory <- list.files("./data/example_data_prep_calc",
                      recursive = T,
                      full.names = T)
## Extraction of ID for 2 types of loggers from file names (TMS, HOBO)
jmena <- basename(soubory) # file names
logger_id <-
  str_match(jmena, "data_\\s*(.*?)\\s*_") # extraction of TOMST TMS ID
hob <-
  is.na(logger_id[, 2]) # which loggers don't have TMS ID, those are HOBO
logger_id[hob] <-
  substr(jmena[hob], 1, 8) # extraction of HOBO logger IDs

## Extraction of reading year from folder name where data is located
odecty <- basename(dirname(soubory))

## Now we know the path, logger ID and reading year.
ft <- data.frame(path = soubory,
                 logger_id = logger_id[, 2],
                 odecty = odecty)

## However, we still lack information about location, logger type and date format
## we get this information from the field records table
## we need a key of which loggers were at which location in which year
## these are columns 3,5,7
zaznamy.dlouha <- pivot_longer(zaznamy,
  cols = c(3, 5, 7),
  names_to = "odecty",
  values_to = "logger_id")

## columns with notes are not needed now
## we get a table with key locality-logger_type-reading-logger_id
zaznamy.dlouha <- zaznamy.dlouha[,-c(3:5)]

## create "files_table"
## joining location key and path to files by logger ID and reading year
files_table <-
  merge(ft, zaznamy.dlouha, by = c("logger_id", "odecty"))

## derive myClim data type from logger type information
data_format <- files_table$logger_type
data_format[data_format == "Thermo"]  <- "TOMST"
data_format[data_format == "TMS4"]    <- "TOMST"
data_format[data_format == "HOBO_RH"] <- "HOBO"
files_table$data_format <- data_format

## by examining date columns in our logger files
## we find that HOBO files have a consistent date format
## TOMST files have 4 date formats
## date format depends on reading software settings
## the more people and computers involved in downloading data, the more possible formats
date_format <- files_table$data_format
date_format[date_format != "HOBO"] <-
  "%d.%m.%Y %H:%M:%S@%d.%m.%Y %H:%M@%Y.%m.%d %H:%M@%d.%m.%Y"
date_format[date_format == "HOBO"] <- "%d.%m.%Y %H:%M:%S"
files_table$date_format <- date_format

## Keep only necessary columns
files_table <-
  files_table[, c("path", "locality_id", "data_format", "date_format")]

default_kable(head(files_table,10))

# ## Loading microclimatic data into myClim
# ## assigning loggers to locations according to "files_table"
# micro.data <-  mc_read_data(files_table = files_table,
#                silent = F,
#                clean = F)
# 
# ## Loading using files_table prepared manually, saved to .csv file
# micro.data <-  mc_read_data(files_table = "./data/files_table.csv",
#                silent = F,
#                clean = F)
# 
# ## Perform basic machine cleaning and data checking, time step consistency
# ## duplicates, time axis sequence, missing values
# micro.data.clean <- mc_prep_clean(micro.data, silent = T)

# Previous chunk "01-read_tms_data" is turned off, takes too long
# Loading previously prepared output  
micro.data.clean <- mc_load("./data/prep_CZ2_10_clean.rds")

## Printing information about loggers: data range, measurement step, 
## duplicates, order, missing data
info.tms <- mc_info_clean(micro.data.clean)

info.tms.print <- info.tms
info.tms.print$start_date <- substr(info.tms.print$start_date, 1, 10)
info.tms.print$end_date <- substr(info.tms.print$end_date, 1, 10)
colnames(info.tms.print) <-
  c("locality_id", "serial_number", "start_date", "end_date", "step",
    "duplicities", "missing", "disordered", "rounded")
default_kable(head(info.tms.print, 10))

## Static display of temperature and soil moisture progression of TOMST TMS-4 logger 
## Line graph with legend adjusted to 3 rows 
# Use guides() and guide_legend() to set number of rows in legend
mc_plot_line(micro.data.clean[c("CS_26","CZ2_LUZNICE")], # Select locations
             sensors = c("TMS_T3","TMS_moist"), # Use specific sensors
             color_by_logger = TRUE) + ggplot2::guides(color 
                                      = ggplot2::guide_legend(nrow = 3))

## Raster graph
mc_plot_raster(micro.data.clean[c("CS_26","CZ2_LUZNICE")],
               sensors = "TMS_moist")

## Detection of kicked out TMS4 (without soil contact) 
micro.data.prep <- mc_prep_TMSoffsoil(micro.data.clean)

## Visualization of detail of location CS_26 using mc_plot_line 
# and adjust legend to 3 rows 
mc_plot_line(
  micro.data.prep[c("CS_26", "CZ2_LUZNICE")],  
  sensors = c("TMS_moist", "off_soil"),        
  color_by_logger = TRUE                       
) + ggplot2::guides(color = ggplot2::guide_legend(nrow = 3))

## Check for pulling out at all locations 
mc_plot_line(micro.data.prep, sensors = "off_soil")

# ## Installation of myClimGUI
# remotes::install_github("https://github.com/ibot-geoecology/myClimGui")
# 
# ## launching application, loading data into myClimGUI
# myClimGui::mcg_run(micro.data.prep)

## Getting sample "states/tag" table
states <- mc_info_states(micro.data.prep)

## Saving and editing table in MS excel
write.xlsx(states,"./data/statesR.xlsx")

## Loading edited table with new "error" tags
states.insert <- read.xlsx("./data/states_edit.xlsx")

## Adding new tag value to myClim object
micro.data.prep.s <- mc_states_insert(micro.data.prep,states_table = states.insert)

## Replacing tag value "error" with NA (delete)
micro.data.prep.na <- mc_states_replace(micro.data.prep.s, tags = "error", 
                                        replace_value = NA)

## Removing auxiliary sensor "off_soil"
micro.data.prep.na <- mc_filter(micro.data.prep.na,sensors = "off_soil", reverse = T)

default_kable(states.insert)

mc_plot_line(micro.data.prep.na, sensors = "TMS_moist")

## Loading table with calibration values (offset)
calib <- read.xlsx("./data/calibrationR.xlsx", detectDates = FALSE)
calib$datetime <- as.POSIXct(convertToDate(calib$datetime))

## Loading calibration data into myClim
micro.data.prep.c <-
  mc_prep_calib_load(micro.data.prep.na, calib_table = calib)

## Application of calibration factor to time series
micro.data.prp.cal <- mc_prep_calib(micro.data.prep.c)

default_kable(head(calib,10))

# # Joining time series
# micro.data.join <- mc_join(micro.data.prp.cal)
# ## visualization of detail of location CS_26
# ## Due to interactive nature output is not shown here
# mc_plot_line(micro.data.join[c("CS_26","CZ2_LUZNICE")],
#              sensors = c("TMS_moist","TMS_T1"),
#              color_by_logger = TRUE)

# previous chunk "join" doesn't run in rmd because it's interactive 
# therefore loading and printing output here
micro.data.join <- mc_load("./data/prep_CZ2_10_join.rds")

## visualization of detail of location CS_26
mc_plot_line(micro.data.join[c("CS_26","CZ2_LUZNICE")],
             sensors = c("TMS_moist","TMS_T1"),
             color_by_logger = TRUE)

## Filling missing values with maximum length of 10 consecutive measurements
micro.data.fill <- mc_prep_fillNA(micro.data.join,maxgap = 10)

# ## Add longitude in WGS84 coordinate system (lon_wgs84)
# # in decimal degrees
# micro.data.fill <- mc_prep_meta_locality(micro.data.fill,
#                                          list(CZ2_KRKLAB = 15.5640182,
#                                               CZ2_KLINOV = 12.9735495,
#                                               CZ2_BARINY = 17.9556943,
#                                               CZ2_HRUSUT = 17.4393209,
#                                               CZ2_MASTALE = 16.1369899,
#                                               CZ2_HODDUB = 17.0748401,
#                                               CZ2_CEPICKA = 16.3635405,
#                                               CZ2_LUZNICE = 14.8428597,
#                                               CS_26 = 13.9910361,
#                                               CZ2_BUKOVEC = 15.3606035),
#                                          param_name = "lon_wgs84")
# ## Add latitude in WGS84 coordinate system (lat_wgs84)
# # in decimal degrees
# micro.data.fill <- mc_prep_meta_locality(micro.data.fill,
#                                          list(CZ2_KRKLAB = 50.7512076,
#                                               CZ2_KLINOV = 50.3935486,
#                                               CZ2_BARINY = 49.6310008,
#                                               CZ2_HRUSUT = 49.6932542,
#                                               CZ2_MASTALE = 49.8115909,
#                                               CZ2_HODDUB = 48.8803506,
#                                               CZ2_CEPICKA = 49.4982277,
#                                               CZ2_LUZNICE = 48.9940496,
#                                               CS_26 = 50.5953875,
#                                               CZ2_BUKOVEC = 50.8143946),
#                                          param_name = "lat_wgs84")
# ## Saving final myClim object
# mc_save(micro.data.fill, file = "./data/prep_CZ2_10_join.rds")
