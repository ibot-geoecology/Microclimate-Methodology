knitr::opts_chunk$set(echo = TRUE)
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

## Loading prepared myClim object from chapter 1 - Data Preparation from Files
# Installation and loading of libraries
suppressWarnings(lapply(c("myClim", "ggplot2"), function(pkg) {
  if (!require(pkg, character.only = TRUE))
    install.packages(pkg, repos = "https://mirrors.nic.cz/R/")
  library(pkg, character.only = TRUE)
}))

micro.data <- mc_load("./data/prep_CZ2_10_join.rds")
micro.data

## plotting graph of temperature and relative humidity progression
p <- mc_plot_line(micro.data[5:7], sensors = c("TMS_T3", "HOBO_RH"))

## manual color adjustment
p <-
  p + ggplot2::scale_color_manual(values = scales::alpha(c("darkblue", "hotpink"), 0.7),
                                  name = NULL)
print(p)

## calculation of offset relative to UTC, photoperiod synchronization
micro.data<-mc_prep_solar_tz(micro.data)

default_kable(mc_info_meta(micro.data))

# Instalation and loading of libraries
suppressWarnings(lapply(c("myClim", "dplyr", "tidyr"), function(pkg) {
  if (!require(pkg, character.only = TRUE))
    install.packages(pkg, repos = "https://mirrors.nic.cz/R/")
  library(pkg, character.only = TRUE)
}))
# For example we'll use TMS_T3 sensor 15 cm above ground, from TMS-4 TOMST logger
micro.data <- mc_calc_gdd(micro.data, sensor = "TMS_T3", t_base = 5)
micro.data <- mc_calc_fdd(micro.data, sensor = "TMS_T3", t_base = 5)

## display sensors available in myClim object for calculations
levels(factor(mc_info(micro.data)$sensor_name))

## select temperature sensors and required aggregation functions
micro.veget <- mc_agg(micro.data,
                    period = "custom",
                    custom_start = "05-01",
                    custom_end = "08-31",
                    percentiles = c(5,95),
                    min_coverage = 0.9,
                    fun=list(
                      FDD0="sum",
                      GDD5="sum",
                      HOBO_T=c("mean","percentile","range"),
                      Thermo_T=c("mean","percentile","range"),
                      TMS_T1=c("mean","percentile","range"),
                      TMS_T2=c("mean","percentile","range"),
                      TMS_T3=c("mean","percentile","range")))

## display resulting sensors
levels(factor(mc_info(micro.veget)$sensor_name))

## convert from myClim object to simple table, long format
df.veget <- mc_reshape_long(micro.veget)

## visualization
## average, minimum and maximum air temperature 200 cm above ground (Thermo_T)
df.veget$datetime <- as.character(df.veget$datetime)
viz <- df.veget %>%
  filter (
    sensor_name %in% c(
      "Thermo_T_mean",
      "Thermo_T_percentile5",
      "Thermo_T_percentile95"
      
    )
  ) %>%
  filter (!is.na(value))

viz <- pivot_wider(viz, 
                   names_from = "sensor_name",
                   values_from = "value")

p <-
  ggplot(viz, aes(x = locality_id, y = Thermo_T_mean, col = datetime)) +
#  ggplot(viz, aes(x = locality_id, y = TMS_T1_mean, col = datetime)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(ymin = Thermo_T_percentile5,
        ymax = Thermo_T_percentile95,
#    aes(ymin = TMS_T1_percentile5,
#        ymax = TMS_T1_percentile95,
        width = 0.15),
    position = position_dodge(width = 0.5)
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 60,
    vjust = 0.7,
    hjust = 0.5
  )) +
  theme(legend.position = "bottom") +
  labs(title = "Temperature at 2 m", 
       subtitle = "average, minimum (P5) and maximum (P95)")

print(p)

## unify period of TMS and HOBO loggers to 30 min
micro.data30 <- mc_agg(micro.data, period = "30 min", fun = "mean")

## calculation of myClim temperature variables for growing season
micro.temp <-
  mc_env_temp(
    micro.data30,
    period = "year",
    min_coverage = 0.9,
    gdd_t_base = 5,
    fdd_t_base = 0
  )

## remove missing values
micro.temp <- filter(micro.temp, !is.na(value))

## visualization
micro.temp$fun <- micro.temp$sensor_name %>%
  strsplit(".", fixed = T) %>%
  lapply("[", 3) %>%
  unlist()

viz <- micro.temp %>%
  filter (fun %in% c("mean", "min5p", "max95p")) %>%
  filter (!is.na(value))
viz$sensor_name <- NULL
viz <- pivot_wider(viz, names_from = "fun",
                   values_from = "value")

p <- ggplot(viz, aes(x = locality_id, y = mean, col = height)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = min5p,
                    ymax = max95p,
                    width = 0.15),
                position = position_dodge(width = 0.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 60,
    vjust = 0.7,
    hjust = 0.5
  )) +
  theme(legend.position = "bottom") +
  ylab("Temperature [°C]") +
  xlab(NULL) +
  labs(title = "Temperature at different heights/depths",
       subtitle = "average, minimum (P5) and maximum (P95) in year 2022")

print(p)

## calculation of virtual sensor for volumetric moisture
## conversion of TMS moisture signal to volumetric moisture
moist.data <- mc_calc_vwc(micro.data, soiltype = "universal",
                          frozen2NA = TRUE)

## visualization
p <- mc_plot_line(moist.data,sensors = c("TMS_moist", "VWC_moisture"))
p <- p + ggplot2::scale_color_manual(values = c("darkblue","steelblue"), 
                                   name = NULL)
print(p)

## use myClim object with virtual sensor for volumetric moisture
## remove HOBO loggers from myClim object, they have different time step
moist.data.TMS <-
  mc_filter(moist.data, logger_types = "HOBO_U23-001A", reverse = T)

## calculation of variables only for TOMST TMS loggers
micro.moist <-
  mc_env_moist(moist.data.TMS, period = "year", min_coverage = 0.9)

## remove missing values
micro.moist <- filter(micro.moist, !is.na(value))

## visualization
micro.moist$fun <- micro.moist$sensor_name %>%
  strsplit(".", fixed = T) %>%
  lapply("[", 3) %>%
  unlist()
micro.moist$sensor_name <- NULL
micro.moist.w <- pivot_wider(micro.moist, names_from = "fun",
                             values_from = "value") %>%
  filter(datetime == as.Date("2022-01-01"))

ggplot(micro.moist.w, aes(x = locality_id, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = `5p`,
                    ymax = `95p`,
                    width = 0.15)) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 60,
    vjust = 0.7,
    hjust = 0.5
  )) +
  theme(legend.position = "bottom") +
  ylab("Volumetric soil moisture [%]") +
  xlab(NULL) +
  labs(title = "Volumetric soil moisture",
       subtitle = "average, minimum (P5) and maximum (P95) for year 2022")

## keep only HOBO loggers
micro.data.HOBO <- mc_filter(micro.data,logger_types = "HOBO_U23-001A",reverse = F)

## calculation of virtual sensor for vapor pressure deficit (VPD) from HOBO loggers
vpd.data <- mc_calc_vpd(micro.data.HOBO)

## visualization
p <- mc_plot_line(vpd.data,sensors = c("HOBO_RH", "VPD"),scale_coeff = 20)
p <- p + ggplot2::scale_color_manual(values = c("red","black"), 
                                   name = NULL)
print(p)

## use myClim object with virtual VPD sensor calculated above
micro.vpd <- mc_env_vpd(vpd.data, period = "year", min_coverage = 0.9)

## remove missing values
micro.vpd <- filter(micro.vpd, !is.na(value))

## visualization
micro.vpd$proměnná <- micro.vpd$sensor_name %>%
  strsplit(".", fixed = T) %>%
  lapply("[", 3) %>%
  unlist()

ggplot(micro.vpd, aes(x = locality_id, y = value, col = proměnná)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ylab("Vapor pressure deficit [kPa]") +
  xlab(NULL) +
  labs(title = "Vapor pressure deficit",
       subtitle = "average and maximum (P95) for year 2022")

## Snow detection based on ground temperatures (TMS_T2)
## Output saved as virtual sensor "snih"
micro.snow <- mc_calc_snow(micro.data, 
                           sensor = "TMS_T2", 
                           output_sensor = "snih", 
                           range = 1,
                           tmax = 1.25, 
                           days = 3)
## Visualization
mc_plot_line(micro.snow[c("CZ2_BUKOVEC",
                           "CZ2_KRKLAB",
                           "CS_26")], sensors = c("TMS_T2","snih"))

## Calculation of basic snow cover statistics for winter season 2021/2022
## Selection of data for chosen period
micro.snow.2021 <- mc_prep_crop(micro.snow, 
                                start = as.POSIXct("2021-10-01", tz="UTC"), 
                                end = as.POSIXct("2022-06-01", tz="UTC"))

## Calculation of snow cover duration,
## continuous snow cover defined as minimum of 7 consecutive days
tabulka.snih.2021 <- mc_calc_snow_agg(micro.snow.2021, 
                                      snow_sensor = "snih", 
                                      period = 7)

default_kable(tabulka.snih.2021)

## Clean up variables in R environment
try(rm(df.veget, micro.data, micro.data.HOBO,micro.data30,micro.moist,
   micro.temp, micro.veget, moist.data, moist.data.TMS,p,viz,vpd.data, 
   micro.vpd, micro.snow, micro.snow.2021, tabulka.snih.2021))
