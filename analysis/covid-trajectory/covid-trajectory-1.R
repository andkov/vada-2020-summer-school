# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
requireNamespace("config", quietly=TRUE)

# ---- load-sources ------------------------------------------------------------
config <- config::get()
source("./scripts/common-functions.R")        # reporting functions and quick views

# ---- declare-globals --------------------
ggplot2::theme_set(ggplot2::theme_bw())

# ---- declare-functions ---------------------------
compute_epi_timeline <- function(d, n_deaths_first_day = 1) { #}, d_country ){
  # browser()
  # d <- ds_cgrt %>%
  #   # filter(country_code %in% c("ITA","FRA") ) %>%
  #   filter(country_code %in% c("AFG") ) %>%
  # select(country_code, date, n_cases, n_deaths)
  #
  d_out <- d %>%
    # dplyr::filter(country_code %in% unique(d_country$id)) %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(
      # this solution might be vulnerable to cases where some intermediate dates are missed
      n_deaths_cum         = cumsum(tidyr::replace_na(n_deaths,0))
      ,n_cases_cum         = cumsum(tidyr::replace_na(n_cases,0))
      ,n_deaths_cum_per_1m = n_deaths_cum/n_population_2018*1000000
      ,n_cases_cum_per_1m  = n_cases_cum/ n_population_2018*1000000

      ,cutoff_death        = n_deaths_cum >= 1
      ,cutoff_case         = n_cases_cum >= 1
      ,days_since_1death   = cumsum(tidyr::replace_na(cutoff_death,0))
      ,days_since_1case    = cumsum(tidyr::replace_na(cutoff_case,0))
      ,date_of_1death      = lubridate::as_date(ifelse(days_since_1death==1,date, NA))
      ,date_of_1case       = lubridate::as_date(ifelse(days_since_1case==1,date, NA))
      ,date_of_1death      = min(date_of_1death, na.rm =T)
      ,date_of_1case       = min(date_of_1case, na.rm =T)
      ,days_since_1death   = (date - date_of_1death) %>% as.integer()
      ,days_since_1case    = (date - date_of_1case) %>% as.integer()

    ) %>%
    dplyr::ungroup() %>%
    # dplyr::filter(epi_timeline > 0) %>%
    dplyr::mutate(
      days_since_exodus    = as.integer(date - lubridate::date("2020-01-13")) # first case outside of china
      ,days_since_pandemic = as.integer(date - lubridate::date("2020-03-11")) # WHO declares pandemic
    ) %>%
    select(-cutoff_death, - cutoff_case, -date_of_1death, -date_of_1case)
  return(d_out)
}

# for testing the function:
# d_out <- ds0 %>%  filter(country_code == "ITA") %>%
#     select(
#       country_code, date,n_cases, n_deaths, ConfirmedDeaths, ConfirmedCases
#     ) %>%
#   compute_epi_timeline()


# ---- load-data -------------------------------------------------------------
# list of focal countries
ds_country <- readr::read_csv(config$path_country) %>%
  dplyr::filter(desired)

ds_covid <- readr::read_csv(config$path_input_covid)
# ds_covid %>% glimpse()

ds_country_codes <- readr::read_csv(config$path_country_codes)

# ---- inspect-data ----------------------

# ---- tweak-data-1 ---------------
ds0 <- ds_covid %>%
  compute_epi_timeline() %>%
  filter(
    country_code %in% (ds_country %>% filter(desired) %>% pull(id))
  ) %>%
  dplyr::left_join(
    ds_country_codes,
    by = c("country_code" = "country_code3")
  )
ds0 %>% glimpse()
d_out <- ds0 %>% filter(country_code == "ITA")
# ----- q1-basic-timeline -------------
# ds0 %>% glimpse()
d1 <- ds0 %>%
  filter(country_code %in% ds_country$id)
g1 <- d1 %>%
  ggplot(aes(
    x = days_since_exodus
    ,y = n_cases_cum
    # ,y =n_cases_cum_per_1m
    # ,y = n_deaths_cum
    # ,y = n_deaths_cum_per_1m
  ))+
  geom_line()+
  # geom_line(aes(y=StringencyIndex), color = "red")+
  facet_wrap(~country_label, scale = "free")+
  geom_point(data = d1 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d1 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  labs(
    title = "Timeline of COVID-19 "
    , y = "Cumulative Cases", x = "Days since first case outside of China (Jan 13, 2020)"
    , caption = "(first dot) = 1st confirmed case, (second dot) = 1st confirmed death, (dashed line) = pandemic announced by WHO"
  )+
  geom_vline(xintercept = 58, linetype = "dashed")
cat("\n## Cases\n")
g1
cat("\n## Cases per 1m\n")
g1 + aes(y = n_cases_cum_per_1m)+labs(y = "Cumulative Cases per 1 mil")
cat("\n## Deaths\n")
g1 + aes(y = n_deaths_cum)+labs(y = "Cumulative Deaths")
cat("\n## Deaths per 1m\n")
g1 + aes(y = n_deaths_cum_per_1m)+labs(y = "Cumulative Deaths per 1 mil")


# ----- q1a -----------
focal_vars <- c( "n_cases_cum", "n_cases_cum_per_1m", "n_deaths_cum", "n_deaths_cum_per_1m",
                 "StringencyIndex")

ds1 <- ds0 %>%
  filter(country_code %in% ds_country$id) %>%
  # dplyr::filter(country_code %in% c("ITA","FRA")) %>%
  dplyr::select(country_code, country_label, days_since_exodus, days_since_1case,
                days_since_1death,
                n_cases_cum, n_cases_cum_per_1m, n_deaths_cum, n_deaths_cum_per_1m,
                StringencyIndex
  ) %>%
  tidyr::pivot_longer(cols = focal_vars, values_to = "value", names_to = "metric")

print_one_wrap <- function(d, country = "ITA"){
  # d <- ds1; country = "ITA"
  d1 <- d %>% filter(country_code == country)

  g1 <- d1 %>%
    ggplot(aes(x = days_since_exodus, y = value))+
    geom_line()+
    geom_point(
      data = d1 %>% filter(days_since_1case == 1),
      size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
    geom_point(
      data = d1 %>% filter(days_since_1death == 1),
      size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
    geom_vline(xintercept = 58, linetype = "dashed")+
    facet_wrap(country_label ~ metric, scale = "free_y",ncol = 5)
  g1
}

# ds1 %>% print_one_wrap(country = 'ITA')

countries <- unique(ds1$country_code)
for(country_i in countries){
  cat("\n## ", country_i,"\n")
  ds1 %>% print_one_wrap(country = country_i) %>% print()
  cat("\n")
}

# ----- q2-response-trend -----------------
# What the trend response to COVID-10 by each country?

d1 <- ds0 %>%
  filter(country_code %in% ds_country$id)
g1 <- d1 %>%
  ggplot(aes(x = days_since_exodus, y = StringencyIndex))+
  geom_line()+
  geom_point(data = d1 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d1 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  facet_wrap(~country_label)+
  labs(
    title = "Timeline of OECD countries' respones to COVID-19 as measured by the Stringency Index"
    ,y = "Stringency Index", x = "Days since first case outside of China (Jan 13, 2020)"
    , caption = "First dot = 1st confired case, Second dot = 1st confirmed death, line = Pandemic announced by WHO"
  )+
  geom_vline(xintercept = 58, linetype = "dashed")
g1


# ----- q2-all -----------------

d2 <- ds0 %>%
  filter(country_code %in% ds_country$id)
g2 <- d2 %>%
  filter(country_code %in% ds_country$id) %>%
  # filter(country_code == "ITA") %>%
  ggplot(aes(x = days_since_exodus, y = StringencyIndex, group = country_label))+
  geom_line( alpha = .2)+
  geom_point(data = d2 %>% filter(days_since_1case == 1), size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21)+
  geom_point(data = d2 %>% filter(days_since_1death == 1), size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21)+
  labs(
    title = "Timeline of OECD countries' respones to COVID-19 as measured by the Stringency Index"
    ,y = "Stringency Index", x = "Days since first case outside of China (Jan 13, 2020)"
  )+
  geom_vline(xintercept = 58, linetype = "dashed")
g2 <- plotly::ggplotly(g2)
g2

# ---- q3 -----------

# d1 <- ds0 %>%
d_out <- ds0 %>%
  filter(country_code == "ITA") %>%
  select(country_code, date,n_cases_cum, n_deaths_cum, days_since_1case, days_since_1death)

# Deaths 30 days after 1st death
d1 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 30) %>%
  dplyr::select(country_code, n_deaths_cum, n_population_2018) %>%
  dplyr::rename(n_deaths_30days_since_1death = n_deaths_cum) %>%
  dplyr::mutate(n_deaths_30days_since_1death_per100k = n_deaths_30days_since_1death/n_population_2018*100000 ) %>%
  dplyr::select(-n_population_2018)

# Deaths 60 days after 1st death
d2 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1death == 60) %>%
  dplyr::select(country_code, n_deaths_cum,n_population_2018) %>%
  dplyr::rename(n_deaths_60days_since_1death = n_deaths_cum) %>%
  dplyr::mutate(
    n_deaths_60days_since_1death_per100k = n_deaths_60days_since_1death/n_population_2018*100000
  ) %>%
  dplyr::select(-n_population_2018)

# Cases 30 days after 1st case
d3 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 30) %>%
  dplyr::select(country_code, n_cases_cum, n_population_2018) %>%
  dplyr::rename(n_cases_30days_since_1case = n_cases_cum) %>%
  dplyr::mutate(
    n_cases_30days_since_1case_per100k = n_cases_30days_since_1case/n_population_2018*100000
  ) %>%
  dplyr::select(-n_population_2018)

# Cases 60 days after 1st case
d4 <- ds0 %>%
  group_by(country_code) %>%
  dplyr::filter(days_since_1case == 60) %>%
  dplyr::select(country_code, n_cases_cum, n_population_2018) %>%
  dplyr::rename(n_cases_60days_since_1case = n_cases_cum) %>%
  dplyr::mutate(
    n_cases_60days_since_1case_per100k = n_cases_60days_since_1case/n_population_2018*100000
  ) %>%
  dplyr::select(-n_population_2018)

# ds0 %>% filter(country_ == "LVA")
ds_response <- list(d1,d2,d3,d4) %>% Reduce(function(a,b) dplyr::full_join(a,b), .)
ds_response <- ds_response %>%
  dplyr::left_join(
    ds_covid %>% distinct(country_code, geo_id, country)
  ) %>%
  dplyr::filter(!is.na(country_code))

ds_response %>% neat_DT

# ---- publish ---------------------------------------
path_report <- "./analysis/response-stringency-1/response-stringency-1.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


