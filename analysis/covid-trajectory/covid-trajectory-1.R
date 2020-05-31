# This script reads two files: patient event table + location map.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) #For graphing
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(lubridate) # for working with dates
library(plotly) # interactive graphs
# ---- load-sources ------------------------------------------------------------
config <- config::get()

# ---- declare-globals --------------------
# to be applied to every graph we will make
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)
# important dates we will refer to in analysis
date_of_exodus   <- lubridate::date("2020-01-13") # first case outside of China
date_of_pandemic <- lubridate::date("2020-03-11") # WHO declares pandemic
# to help us focus on a manageable set of countries for purposes of demonstration
oecd_countries <- c(
  "AUS", "AUT", "BEL", "CAN", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR",
  "LVA", "LTU", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK",
  "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "RUS", "ZAF"
)
# adds neat styling to your knitr table
neat <- function(x, output_format = "html"){
  # knitr.table.format = output_format
  if(output_format == "pandoc"){
    x_t <- knitr::kable(x, format = "pandoc")
  }else{
    x_t <- x %>%
      # x %>%
      # knitr::kable() %>%
      knitr::kable(format=output_format) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed","responsive"),
        # bootstrap_options = c( "condensed"),
        full_width = F,
        position = "left"
      )
  }
  return(x_t)
}

# ---- declare-functions ---------------------------

# ---- preview-0 ---------------------------------
library(magrittr) #Pipes
library(ggplot2) #For graphing
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(lubridate) # for working with dates
library(plotly) # interactive graphs
config <- config::get()
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)
# important dates we will refer to in analysis
date_of_exodus   <- lubridate::date("2020-01-13") # first case outside of China
date_of_pandemic <- lubridate::date("2020-03-11") # WHO declares pandemic
# to help us focus on a manageable set of countries for purposes of demonstration
oecd_countries <- c(
  "AUS", "AUT", "BEL", "CAN", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR",
  "LVA", "LTU", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK",
  "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "RUS", "ZAF"
)
# to have a handy filter
d_preview <-readr::read_csv(config$path_input_covid) %>%
  mutate(
    oecd = country_code %in% oecd_countries
  ) %>%
  ungroup() %>%
  group_by(country_code) %>%
  mutate(
    # compute timeline of cumulative confirmed cases
    n_cases_cum  = cumsum(tidyr::replace_na(n_cases,0))
    ,date_of_1case = ifelse(cumsum(n_cases_cum > 0) == 1L, date, NA) %>%
      min(na.rm=T) %>%
      lubridate::as_date()
    ,days_since_1case = (date - date_of_1case) %>% as.integer()
    # compute timeine of cumulative deaths
    ,n_deaths_cum  = cumsum(tidyr::replace_na(n_deaths,0))
    ,date_of_1death = ifelse(cumsum(n_deaths_cum > 0) == 1L, date, NA) %>%
      min(na.rm=T) %>%
      lubridate::as_date()
    ,days_since_1death = (date - date_of_1death) %>% as.integer()
    # compute absolute timeline
    ,days_since_exodus   = as.integer(date - date_of_exodus) # first case outside of china
    ,days_since_pandemic = as.integer(date - date_of_pandemic) # WHO declares pandemic
    ,n_deaths_cum_per_1m = as.integer(n_deaths_cum/n_population_2018*1000000)
    ,n_cases_cum_per_1m  = as.integer(n_cases_cum/ n_population_2018*1000000)
  ) %>%
  ungroup() %>%
  select(-date_of_1case, -date_of_1death) %>%
  select(
    date, country_code, n_cases, n_deaths, n_cases_cum, n_deaths_cum,
    days_since_1case, days_since_1death, days_since_exodus, days_since_pandemic,
    dplyr::everything()
  )
# d_preview %>% glimpse()
# ---- preview-1 -----------------
library(plotly)
d_preview %>% filter(oecd) %>%
  highlight_key(~country_label )%>%
  plot_ly(
    x = ~ date, y = ~ log(n_cases_cum)
  ) %>%
  group_by(country_label) %>%
  add_lines() %>%
  highlight(
    on = "plotly_click"
    ,selectize = TRUE
    ,dynamic = TRUE
    ,persistent = TRUE
  )


# ----- preview-2 --------------------------
d1 <- d_preview %>% filter(oecd)
d1 %>%
  ggplot(aes(
    x = days_since_exodus
    ,y = n_deaths_cum_per_1m
  ))+
  geom_line()+
  facet_wrap(~country_label, scale = "free", ncol = 6)+
  geom_point(
    data  = d1 %>% filter(days_since_1case == 1)
    ,size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21
  )+
  geom_point(
    data  = d1 %>% filter(days_since_1death == 1)
    ,size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21
  )+
  geom_vline(xintercept = 58, linetype = "dotted",)+
  geom_vline(xintercept = 75, linetype = "dashed", alpha = .5)+
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", alpha = .5)+
  scale_x_continuous(breaks = seq(0,100, 50))+
  labs(
    title    = "Timeline of COVID-19: Cumulative Deaths per 1 million"
    ,y       = "Cumulative Deaths per 1 mil"
    ,x       = "Days since first case outside of China (Jan 13, 2020)"
    ,caption = "(first dot) = 1st confirmed case, (second dot) = 1st confirmed death,
    (dotted line) = pandemic announced by WHO, (dashed lines) = 75 and 100th day since Exodus"
  )


# ----- preview-3 ----------------
# How long did it take to show first case/death?
d_preview %>%
  filter(oecd) %>%
  filter(days_since_1case == 0) %>%
  filter(!is.na(days_since_1death)) %>% # with a least 1 confirmed death
  filter(!is.na(country_label)) %>%
  mutate(
    country_label                = forcats::fct_reorder(country_label, days_since_exodus)
    ,days_to_1death_since_exodus = (-1*days_since_1death) +days_since_exodus
  ) %>%
  # ggplot(aes(x = days_since_exodus, y = country_label))+
  ggplot(aes(x = days_since_exodus, y = country_label))+
  geom_segment(aes(yend = country_label, xend = 0), linetype=NA, alpha = .8)+
  geom_segment(aes(yend = country_label, xend = days_to_1death_since_exodus, color = "red"))+
  geom_point(shape = 21, size =2, alpha = .6, fill = "#1b9e77")+
  geom_point(aes(x = days_to_1death_since_exodus), shape = 21, size =2, alpha = .6, fill = "#d95f02")+
  geom_text(aes(label = country_code2, x = days_to_1death_since_exodus), hjust = -1, size = 3, color = "grey60")+
  scale_x_continuous(breaks = seq(0,140, 20))+
  guides(color = F)+
  labs(
    title = "COVID Timeline: Days to 1st Case and 1st Death"
    ,x = "Days to first case since exodus (January 13)", y = NULL
    ,caption = "(green dot) = 1st confirmed case, (orange dot) = 1st confirmed death"
  )



# ---- load-data -------------------------------------------------------------
# covid data
ds_covid <- readr::read_csv(config$path_input_covid)
ds_covid %>% glimpse()
# note that only `date`, `n_cases`, and `n_deatsh` change with time
# other variables have the same value within each country

# ----- tweak-data-1 -------------------
# to help us focus on a smaller subset of countries
oecd_countries <- c(
  "AUS", "AUT", "BEL", "CAN", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR",
  "LVA", "LTU", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK",
  "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "RUS", "ZAF"
)
# to have a handy filter
ds_covid <- ds_covid %>%
  mutate(
    oecd = country_code %in% oecd_countries
  )


# ----- goal_1-1 -------------------
ds_covid <- ds_covid %>%
  group_by(country_code) %>%
  mutate(
    n_cases_cum = cumsum(n_cases) # would produce NA after en
    # n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
  ) %>%
  ungroup()
ds_covid %>%
  filter(country_code == "FIN") %>% filter(date > as_date("2020-02-26")) %>%
  select(date, country_code, n_cases, n_cases_cum)

# ----- goal_1-2 -------------------
ds_covid <- ds_covid %>%
  group_by(country_code) %>%
  mutate(
    # n_cases_cum = cumsum(n_cases) # would produce NA after en
    n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
  ) %>%
  ungroup()
ds_covid %>%
  filter(country_code == "FIN") %>% filter(date > as_date("2020-02-26")) %>%
  select(date, country_code, n_cases, n_cases_cum)

# ----- goal_1-3 ---------------
library(plotly)
g1 <- ds_covid %>%
  filter(oecd) %>%
  highlight_key(~country_label )%>%
  plot_ly(
    x = ~ date, y = ~ log(n_cases_cum)
  ) %>%
  group_by(country_label) %>%
  add_lines()
g1 %>%
  highlight(
    on = "plotly_click"
    ,selectize = TRUE
    ,dynamic = TRUE
    ,persistent = TRUE
  )

# ----- goal_1-4 ---------------
g1 <- ds_covid %>%
  filter(oecd) %>%
  ggplot(aes(x = date, y= log(n_cases_cum)))+
  geom_line(aes(group = country_code))

g1 <- plotly::ggplotly(g1) %>% plotly::highlight(on = "plotly_hover")
g1

# ---- reprex-1 ----------------------
# create reproducible example (reprex) to test out your function
d_reprex <- tibble::tribble(
  ~country_code, ~date, ~n_cases,
  "Alabnia", "2020-03-01", NA,
  "Alabnia", "2020-03-02", 0,
  "Alabnia", "2020-03-03", 1,
  "Alabnia", "2020-03-04", 0,
  "Alabnia", "2020-03-05", 3,
  "Botswana",   "2020-04-01", 0,
  "Botswana",   "2020-04-02", NA,
  "Botswana",   "2020-04-03", 2,
  "Botswana",   "2020-04-04", 3,
  "Botswana",   "2020-04-05", 0,
  "Chile",   "2020-05-01", 2,
  "Chile",   "2020-05-02", 0,
  "Chile",   "2020-05-03", 0,
  "Chile",   "2020-05-04", 3,
  "Chile",   "2020-05-05", 1,
 ) %>%
  mutate(
    date = lubridate::as_date(date)
  )
d_reprex  %>% neat()

# ---- reprex-2 ----------------------
d_reprex_timeline <- d_reprex %>%
  group_by(country_code) %>%
  mutate(
    n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
  )
d_reprex_timeline

# ---- reprex-3 ----------------------
d_reprex_timeline <- d_reprex %>%
  group_by(country_code) %>%
  mutate(
    n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
    ,onset_case = n_cases_cum > 0
  )
d_reprex_timeline

# ---- reprex-4 ----------------------
d_reprex_timeline <- d_reprex %>%
  group_by(country_code) %>%
  mutate(
    n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
    ,onset_case = n_cases_cum > 0
    ,first_case = cumsum(onset_case) == 1L
  )
d_reprex_timeline

# ---- reprex-5 ----------------------
d_reprex_timeline <- d_reprex %>%
  group_by(country_code) %>%
  mutate(
    n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
    ,onset_case = n_cases_cum > 0
    ,first_case = cumsum(onset_case) == 1L
    ,date_of_1case1 = ifelse(first_case, date, NA) %>% lubridate::as_date()
  )
d_reprex_timeline

# ---- reprex-6 ----------------------
d_reprex_timeline <- d_reprex %>%
  group_by(country_code) %>%
  mutate(
    n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
    ,onset_case = n_cases_cum > 0
    ,first_case = cumsum(onset_case) == 1L
    ,date_of_1case1 = ifelse(first_case, date, NA) %>% lubridate::as_date()
    ,date_of_1case2 = min(date_of_1case1, na.rm = T)
  )
d_reprex_timeline

# ---- reprex-7 ----------------------
d_reprex_timeline <- d_reprex %>%
  group_by(country_code) %>%
  mutate(
    n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
    ,onset_case = n_cases_cum > 0
    ,first_case = cumsum(onset_case) == 1L
    ,date_of_1case1 = ifelse(first_case, date, NA) %>% lubridate::as_date()
    ,date_of_1case2 = min(date_of_1case1, na.rm = T)
    ,days_since_1case = (date - date_of_1case) %>% as.integer()
  )
d_reprex_timeline
# ---- reprex-7 ----------------------
d_reprex_timeline <- d_reprex %>%
  group_by(country_code) %>%
  mutate(
    n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
    #,onset_case = n_cases_cum > 0
    # ,first_case = cumsum(onset_case) == 1L
    # ,date_of_1case1 = ifelse(first_case, date, NA) %>% lubridate::as_date()
    # ,date_of_1case2 = min(date_of_1case1, na.rm = T)
    # alternatively, as a single sentence:
    ,date_of_1case = ifelse(cumsum(n_cases_cum > 0) == 1L, date, NA) %>%
      min(na.rm=T) %>%
      lubridate::as_date()
    # relative timeline
    ,days_since_1case = (date - date_of_1case) %>% as.integer()
    ) %>%
  ungroup() %>%
  select(-date_of_1case) # because it is easily derived
d_reprex_timeline %>% neat()


# ---- compute-epi-timeline ------------

ds_covid_timeline <- ds_covid %>%
  group_by(country_code) %>%
  mutate(
    # compute timeline of cumulative confirmed cases
    n_cases_cum  = cumsum(tidyr::replace_na(n_cases,0))
    ,date_of_1case = ifelse(cumsum(n_cases_cum > 0) == 1L, date, NA) %>%
      min(na.rm=T) %>%
      lubridate::as_date()
    ,days_since_1case = (date - date_of_1case) %>% as.integer()
    # compute timeine of cumulative deaths
    ,n_deaths_cum  = cumsum(tidyr::replace_na(n_deaths,0))
    ,date_of_1death = ifelse(cumsum(n_deaths_cum > 0) == 1L, date, NA) %>%
      min(na.rm=T) %>%
      lubridate::as_date()
    ,days_since_1death = (date - date_of_1death) %>% as.integer()
    # compute absolute timeline
    ,days_since_exodus   = as.integer(date - date_of_exodus) # first case outside of china
    ,days_since_pandemic = as.integer(date - date_of_pandemic) # WHO declares pandemic
    ,n_deaths_cum_per_1m = as.integer(n_deaths_cum/n_population_2018*1000000)
    ,n_cases_cum_per_1m  = as.integer(n_cases_cum/ n_population_2018*1000000)
  ) %>%
  ungroup() %>%
  select(-date_of_1case, -date_of_1death) %>%
  select(
    date, country_code, n_cases, n_deaths, n_cases_cum, n_deaths_cum,
    days_since_1case, days_since_1death, days_since_exodus, days_since_pandemic,
    dplyr::everything()
  )
ds_covid_timeline %>% glimpse()

# ----- goal_2-1 ---------------------
g1 <- d1 %>%
  ggplot(aes(
    x = days_since_exodus
    ,y = n_cases_cum
  ))+
  geom_line()+
  facet_wrap(~country_label, scale = "free", ncol = 6)+
  geom_point(
    data  = d1 %>% filter(days_since_1case == 1)
    ,size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21
  )+
  geom_point(
    data  = d1 %>% filter(days_since_1death == 1)
    ,size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21
  )+
  geom_vline(xintercept = 58, linetype = "dotted",)+
  geom_vline(xintercept = 75, linetype = "dashed", alpha = .5)+
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", alpha = .5)+
  scale_x_continuous(breaks = seq(0,100, 50))+
  labs(
    title = "Timeline of COVID-19: Cumulative Cases"
    , y = "Cumulative Cases (in thousands)", x = "Days since first case outside of China (Jan 13, 2020)"
    , caption = "(first dot) = 1st confirmed case, (second dot) = 1st confirmed death,
    (dotted line) = pandemic announced by WHO, (dashed lines) = 75 and 100th day since Exodus"
  )
cat("\n## Cases\n")
g1
# cat("\n## Cases per 1m\n")
# g1 + aes(y = n_cases_cum_per_1m)+labs(y = "Cumulative Cases per 1 mil (in thousands)",
#                                       title = "Timeline of COVID-19: Cumulative Cases per 1 million")
# cat("\n## Deaths\n")
# g1 + aes(y = n_deaths_cum)+labs(y = "Cumulative Deaths",
#                                 title = "Timeline of COVID-19: Cumulative Deaths")
# cat("\n## Deaths per 1m\n")
# g1 + aes(y = n_deaths_cum_per_1m)+labs(y = "Cumulative Deaths per 1 mil",
#                                        title = "Timeline of COVID-19: Cumulative Deaths per 1 million")
# ---- goal_3-1 ---------------------

g3 <- ds_covid_timeline %>%
  filter(oecd) %>%
  filter(days_since_1case == 0) %>%
  mutate(
    country_label                = forcats::fct_reorder(country_label, days_since_exodus)
    ,days_to_1death_since_exodus = (-1*days_since_1death) + days_since_exodus
  ) %>%
  ggplot(aes(x = days_since_exodus, y = country_label))+
  geom_segment(aes(yend = country_label, xend = 0), linetype=NA, alpha = .8)+
  geom_segment(aes(yend = country_label, xend = days_to_1death_since_exodus, color = "red"))+
  geom_point(shape = 21, size =2, alpha = .6, fill = "#1b9e77")+
  geom_point(aes(x = days_to_1death_since_exodus), shape = 21, size =2, alpha = .6, fill = "#d95f02")+
  geom_text(aes(label = country_code2, x = days_to_1death_since_exodus), hjust = -1, size = 3, color = "grey60")+
  scale_x_continuous(breaks = seq(0,140, 20))+
  guides(color = F)+
  labs(
    title = "COVID Timeline: Days to 1st Case and 1st Death"
    ,x = "Days to first case since exodus (January 13)", y = NULL
    ,caption = "(green dot) = 1st confirmed case, (orange dot) = 1st confirmed death"
  )
g3

# ---- publish ---------------------------------------
path_report <- "./analysis/covid-trajectory/covid-trajectory-1.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


