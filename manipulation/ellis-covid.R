# Source
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide



rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.


# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(utils)
library(httr)
library(magrittr)
library(dplyr)

# ---- declare-globals ---------------------------------------------------------
config <- config::get()
path_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

# ---- load-data ---------------------------------------------------------------

#download the dataset from the ECDC website to a local temporary file
GET(url = path_url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
ds_ocdc_raw <- read.csv(tf)
if (!fs::dir_exists(fs::path_dir(config$path_input_covid))) fs::dir_create(fs::path_dir(config$path_input_covid))
readr::write_csv(ds_ocdc_raw, config$path_input_covid)
checkmate::assert_file(config$path_input_covid)
# # run above line once per update

ds_covid <- readr::read_csv(config$path_input_covid)
ds_covid %>% glimpse()

# ---- reconcile-countries -------------------
ds_country <-
  readr::read_csv(
    config$path_country
  ) %>%
  dplyr::filter(desired)

ds_country <-
  ds_country %>%
  dplyr::left_join(
    ds_covid %>%
      dplyr::distinct(countriesAndTerritories,countryterritoryCode),
    by = c("id" = "countryterritoryCode")
  )
# sources can be joined by the three letter country code
# ---- tweak-data -----------------------
names(ds_covid) <- c("date", "day", "month", "year", "n_cases", "n_deaths", "country", "geo_id", "country_code","n_population_2018", "continent")

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


