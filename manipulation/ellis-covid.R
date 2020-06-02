# Data Source
# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
rm(list=ls(all=TRUE)) # clear the memory of variables from previous run.
cat("\f") # clear console when working in RStudio

# ---- load-sources -----------------------------------------

# ---- load-packages ----------------------------------------
library(magrittr) #Pipes
library(readr)
library(dplyr)
library(fs)

# ---- declare-globals ---------------------------------------------------------
config   <- config::get()
path_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"

# ---- load-data ---------------------------------------------
# download the dataset from the ECDC website to a local temporary file
ds_ecdc_raw <- readr::read_csv(path_url)

# to create the folder if it does not exist
if( !fs::dir_exists(  fs::path_dir(config$path_input_covid)  ) ){
  fs::dir_create( fs::path_dir(config$path_input_covid) )
}
ds_ecdc_raw %>% readr::write_csv(config$path_input_covid) # to save a local version
checkmate::assert_file(config$path_input_covid) # to verify success
rm(ds_ecdc_raw)
# run above line once per update

# input local saved file for the current date
ds0 <- readr::read_csv(config$path_input_covid)
ds0 %>% glimpse()

# ---- tweak-data-1 -----------------------
ds1 <- ds0 # to preserve the original copy for tracing and debugging
names(ds1) <- names(ds1) %>% snakecase::to_snake_case()

ds1 <- ds1 %>%
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
    date = lubridate::dmy(date) # to be interpreted as a date in computation/graphing
  )
ds1 %>% glimpse()

# ---- tweak-data-2 ---------------------
ds2 <- ds1 # to preserve the original copy for tracing and debugging
# to verify the presence of country identification
ds2 %>% pull(country_code) %>% unique()
ds2 %>% pull(country_code2) %>% unique()
ds2 %>% pull(country_label) %>% unique()
# to view what we miss
ds2 %>%
  filter(
    country_code == "N/A"  |
      is.na(country_code)  |
      is.na(country_code2) |
      country_code2 == "JPG11668"
    ) %>%
  distinct(country_code2, country_code, country_label)

# futher tweak based on manual inspection
ds2 <- ds2 %>%
  filter( !country_code2 %in% c("AI", "BQ", "JPG11668", "FK", "EH") ) %>%
  mutate(
    country_code2 = ifelse(country_code == "NAM", "NA", country_code2)
  )

# ----- tweak-data-3 ----------------------------
ds3 <- ds2 # to preserve the original copy for tracing and debugging
# we observe that some countries do not have all dates reported:
ds3 %>%
  filter(country_code == "AFG") %>%
  filter(date > lubridate::ymd("2020-03-01"))%>%
  arrange(date)

# in order to have the complete timeline:
dates     <- min(ds3$date):max(ds3$date) %>% lubridate::as_date()
countries <- unique(ds3$country_code) %>% na.omit()
ds_dates  <- tibble::as_tibble(
  expand.grid(
    "date" = dates, "country_code" = countries, stringsAsFactors = F
  )
)
ds_dates %>% glimpse()
ds3 %>% glimpse()

# if we join now, we will have missing values on static variables:
ds_dates %>%
  dplyr::left_join(ds3, by = c("date","country_code") ) %>%
  filter(country_code == "AFG") %>%
  filter(date > lubridate::ymd("2020-03-01"))
# to keep only those variables that change with date
d_timeline <- ds_dates %>%
  dplyr::left_join(
    ds3 %>%
      select("date", "country_code","n_cases", "n_deaths")
    ,by = c("date","country_code")
  )
# to keep only those variabels that DO NOT change with date
d_country_info <- ds3 %>%
  distinct(
    n_population_2018, country_code, country_code2, country_label, continent_label
  )
# to inspect before merging
d_timeline %>% glimpse()
d_country_info %>% glimpse()
# to create a single dataset for subsequent analysis (and possible wrangling)
ds_covid <- dplyr::left_join(d_timeline, d_country_info, by = "country_code")
ds_covid <- ds_covid %>% select(date, n_cases, n_deaths, country_code, dplyr::everything())
# to inspect
ds_covid %>%
  filter(country_code == "AFG") %>%
  filter(date > lubridate::ymd("2020-03-01"))

# ---- save-to-disk ------------------------
ds_covid %>% readr::write_csv(config$path_input_covid)

# saved on June 2 to create a stable data source for the workshop:
# ds_covid %>% readr::write_csv("./data-public/derived/covid-2020-06-02.csv")
