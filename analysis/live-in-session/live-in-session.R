rm(list=ls(all=TRUE))  # clear the variables from previous runs
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) #For graphing
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(lubridate) # for working with dates
library(plotly) # interactive graphs
library(crosstalk) # interactivity in graphs

# ---- declare-globals --------------------
# settings to be applied to every graph we will make
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
focus_countries <- c("CAN","USA","ITA","TUR", "NLD","CHE")

# ---- load-data -------------------------------------------------------------
# covid data
ds_covid <- readr::read_csv("https://github.com/andkov/vada-2020-summer-school/raw/master/data-public/derived/covid-2020-06-01.csv")

# ---- inspect-data-1 ------------------------------
ds_covid %>% glimpse()
# note that only `date`, `n_cases`, and `n_deatsh` change with time
# other variables have the same value within each country
ds_covid %>%
  filter(country_code == "FIN") %>%
  filter(date > as_date("2020-02-26"))

# ----- tweak-data-1 -------------------
# to have a handy filter
ds_covid <- ds_covid %>%
  mutate(
    oecd   = country_code %in% oecd_countries
    ,focus = country_code %in% focus_countries
  )

# ----- goal_1-dev ---------------
# Subgoals
# 1. Make ggplot
# 2. Enhance with plotly


# ----- goal_1-solution ---------------
g1 <-
  ds_covid %>%
  filter(oecd) %>%
  plotly::highlight_key(~ country_label) %>% # BEFORE bulding the plot!
  ggplot(
    aes(
      x  = date
      ,y = n_cases %>% log()
      ,group = country_label
    )
  )+
  geom_line()
# g1
g1p <-
  plotly::ggplotly(g1) %>%                   # make into a plotly object
  plotly::highlight(                         # add highlight functionality
    on             = "plotly_click"          # or "plotly_hover"
    ,dynamic       = TRUE                    # adds color option
    ,selectize     = TRUE                    # select what to highlight
    ,defaultValues = "Canada"                # highlights in the beginning
  ) %>%
  plotly::layout(margin = list(l = 0, r = 0, b = 100, t = 0, pad = 0))
g1p

# ---- reprex-data ----------------------
# create reproducible example (reprex) to test out your function
d_reprex <- tibble::tribble(
  ~country_code, ~date, ~n_cases,
  "Alabnia",  "2020-03-01", NA,
  "Alabnia",  "2020-03-02", 0,
  "Alabnia",  "2020-03-03", 1,
  "Alabnia",  "2020-03-04", 0,
  "Alabnia",  "2020-03-05", 3,
  "Botswana", "2020-04-01", 0,
  "Botswana", "2020-04-02", NA,
  "Botswana", "2020-04-03", 2,
  "Botswana", "2020-04-04", 3,
  "Botswana", "2020-04-05", 0,
  "Chile",    "2020-05-01", 2,
  "Chile",    "2020-05-02", 0,
  "Chile",    "2020-05-03", 0,
  "Chile",    "2020-05-04", 3,
  "Chile",    "2020-05-05", 1,
 ) %>%
  mutate(
    date = lubridate::as_date(date)
  )
d_reprex

# ---- reprex-dev ----------------------
# Subgoals:
# 1. Comulative cases (Running total)
# 2. Onset marker
# 3. First case marker
# 4. Date of first case
# 5. Number of days since 1st case


# ---- reprex-solution ----------------------
d_reprex_timeline <- d_reprex %>%
  group_by(country_code) %>%
  mutate(
    n_cases_cum = cumsum(tidyr::replace_na(n_cases,0))
    ,date_of_1case = ifelse(cumsum(n_cases_cum > 0) == 1L, date, NA) %>%
      min(na.rm=T) %>%
      lubridate::as_date()
    # relative timeline
    ,days_since_1case = (date - date_of_1case) %>% as.integer()
    ) %>%
  ungroup()
d_reprex_timeline


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
    ,days_since_exodus   = as.integer(date - date_of_exodus)
    ,days_since_pandemic = as.integer(date - date_of_pandemic)
    # create measures adjusted for population size
    ,n_deaths_cum_per_1m = as.integer(n_deaths_cum/n_population_2018*1000000)
    ,n_cases_cum_per_1m  = as.integer(n_cases_cum/ n_population_2018*1000000)
  ) %>%
  ungroup() %>%
  select(
    date, country_code,
    n_cases, n_deaths, n_cases_cum, n_deaths_cum, n_cases_cum_per_1m, n_deaths_cum_per_1m,
    days_since_1case, days_since_1death, days_since_exodus, days_since_pandemic,
    dplyr::everything()
  )
ds_covid_timeline %>% glimpse()

# ----- goal_2-dev ---------------------
# Subgoals
# 1. Line together
# 2. Lines apart on facets (play with freeing scales)
# 3. Dot for 1st case
# 4. Dot for 1st death
# 5. Play with Absolute vs Relative timeline


# ----- goal_2-solution ---------------------
d2 <- ds_covid_timeline %>% filter(focus)
g2 <-
  d2 %>%
  filter(focus) %>%
  ggplot(aes(
    x = days_since_exodus
    ,y = n_cases_cum
  ))+
  geom_line(aes(group = country_code))+
  facet_wrap(~country_label, scale = "free", ncol = 3)+
  geom_point(
    data  = d2 %>% filter(days_since_1case == 1)
    ,size = 2, fill = "#1b9e77", color = "black", alpha = .5, shape = 21
  )+
  geom_point(
    data  = d2 %>% filter(days_since_1death == 1)
    ,size = 2, fill = "#d95f02", color = "black", alpha = .5, shape = 21
  )+
  geom_vline(xintercept = 58, linetype = "dotted",)+
  geom_vline(xintercept = 75, linetype = "dashed", alpha = .5)+
  geom_vline(xintercept = 100, linetype = "dashed", color = "red", alpha = .5)+
  scale_x_continuous(breaks = seq(0,100, 50))+
  labs(
    title = "Timeline of COVID-19: Cumulative Cases"
    ,y = "Cumulative Cases (in thousands)"
    ,x = "Days since first case outside of China (Jan 13, 2020)"
    ,caption = "(first dot) = 1st confirmed case, (second dot) = 1st confirmed death,
    (dotted line) = pandemic announced by WHO, (dashed lines) = 75 and 100th day since Exodus"
  )
g2
# you can overwrite the mapping to plot a different measure:
g2 +
  aes(y = n_deaths_cum_per_1m)+
  labs(
    y = "Cumulative Deaths per 1 mil"
    ,title = "Timeline of COVID-19: Cumulative Deaths per 1 million"
  )
# ---- goal_3-dev ---------------------
# steps
# 1. dots for first case
# 2. reorder y axis
# 3. dots for first death
# 4. Segment from 1st case to 1st death
# 5. Country codes next to 1st death

# ---- goal_3-solution ---------------------
# finished graph with some added options
g3 <- ds_covid_timeline %>%
  filter(oecd) %>%
  filter(days_since_1case == 0) %>%
  mutate(
    country_label                = forcats::fct_reorder(country_label, days_since_exodus)
  ) %>%
  ggplot(aes(x = date, y = country_label))+
  geom_point(shape = 21, size =2, alpha = .6, fill = "#1b9e77")+
  geom_point(aes(x = date_of_1death), shape = 21, size =2, alpha = .6, fill = "#d95f02")+
  geom_segment(aes(yend = country_label, xend = date_of_1death, color = "red"))+
  geom_text(aes(label = country_code2, x = date_of_1death), hjust = -1, size = 3, color = "grey60")+
  # scale_x_continuous(breaks = seq(0,140, 20))+
  guides(color = F)+
  labs(
    title = "COVID Timeline: Days to 1st Case and 1st Death"
    ,x = "Days to first case since exodus (January 13)", y = NULL
    ,caption = "(green dot) = 1st confirmed case, (orange dot) = 1st confirmed death"
  )
g3

# ---- publish ---------------------------------------
path_report <- "./analysis/live-in-session/live-in-session-narrated.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


