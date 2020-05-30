# Data Source
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(readr)
library(dplyr)
library(fs)

# ---- declare-globals ---------------------------------------------------------
config   <- config::get()
path_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"


# ---- load-data ---------------------------------------------------------------
# # download the dataset from the ECDC website to a local temporary file
# ds_ocdc_raw <- readr::read_csv(path_url)
#
# # to create the folder if it does not exist
# if( !fs::dir_exists(  fs::path_dir(config$path_input_covid)  ) ){
#   fs::dir_create( fs::path_dir(config$path_input_covid) )
# }
# ds_ocdc_raw %>% readr::write_csv(config$path_input_covid) # to save a local version
# checkmate::assert_file(config$path_input_covid) # to verify success
# rm(ds_ocdc_raw)
# # run above line once per update

# input local saved file for the current date
ds_covid <- readr::read_csv(config$path_input_covid)
ds_covid %>% glimpse()

# ---- tweak-data -----------------------
names(ds_covid) <- names(ds_covid) %>% snakecase::to_snake_case()

ds_covid <- ds_covid %>%
  dplyr::rename(
    "date"               = "date_rep"      # because shorter
    ,"n_cases"           = "cases"         # because count people
    ,"n_deaths"          = "deaths"        # because count people
    ,"n_population_2018" = "pop_data_2018" # because count people

    ,"country_label"     = "countries_and_territories"
    ,"country_code"      = "countryterritory_code"
    ,"country_code2"     = "geo_id"
    ,"continent_label"   = "continent_exp"
) %>%
  dplyr::select(-day, -month, -year) %>% # because derivative from date
  dplyr::select(date,n_cases, n_deaths, n_population_2018, dplyr::everything()) %>%
  dplyr::mutate(
    date = lubridate::dmy(date)
  )
ds_covid %>% glimpse()

ds_covid %>% pull(country_code) %>% unique()
ds_covid %>% pull(country_code2) %>% unique()

d <- ds_covid %>% filter(country_code == "N/A")

d <- ds_covid %>% filter(country_code == "NAM") %>%
d

ds_covid <- ds_covid %>%
  dplyr::mutate(
    date = lubridate::dmy(date)
  ) %>%
  dplyr::select(
    country_code,
    date,
    n_cases,
    n_deaths,
    n_population_2018
  ) %>%
  dplyr::filter(!country_code == "N/A")
ds_covid %>% glimpse()
# stem to have the complete timeline
dates     <- min(ds_covid$date):max(ds_covid$date)
countries <- unique(ds_covid$country_code) %>% na.omit()
ds_dates   <- tibble::as_tibble(
  expand.grid(dates, countries, stringsAsFactors = F)
)
names(ds_dates) <- c("date", "country_code")
ds_dates <- ds_dates %>%
  dplyr::mutate(
    date = lubridate::as_date(date)
  )
ds_dates %>% glimpse()
ds_covid %>% glimpse()

ds_covid <- ds_dates %>%
  dplyr::left_join(ds_covid,by = c("date","country_code") ) %>%
  tidyr::fill(n_population_2018) %>%
  dplyr::arrange(country_code, date)


readr::write_csv(ds_covid, config$path_input_covid)

ds_covid <-  ds_covid %>%
  dplyr::filter(country_code %in% unique(ds_country$id))


