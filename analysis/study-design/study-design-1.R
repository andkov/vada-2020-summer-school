rm(list=ls(all=TRUE))  # clear the variables from previous runs
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) #For graphing
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(lubridate) # for working with dates
library(plotly) # interactive graphs
library(crosstalk)
# ---- load-sources ------------------------------------------------------------

# ---- declare-globals --------------------
# to be applied to every graph we will make
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
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
print_wday <- function(x)(x %>% as_date() %>% wday(label=T, abbr=F) %>% as.character())
add_ndays <- function(x,n)(x %>% as_date() + n)
adate <- "2020-01-15"
adate %>% print_wday()
adate %>% add_ndays(1)

add_nsessions <- function(
  x,n,patient_name,final_session=F, score_value = 20, session_modality="f2f"
  ){
  # x  <- "2020-01-15"
  # n  <- 4
  # patient_name <- "A"
  # final_session <- FALSE
  # score_value = c(20,30, 40, 30, 20)
  dout <- as_tibble(
    (as_date(x) + seq(0, n*7, 7))
  ) %>%
    rename(
      date = value
    ) %>%
    mutate(
      session_number = dplyr::row_number(),
      patient = patient_name,
      initial_session = ifelse(session_number ==1, TRUE, FALSE),
      final_session = ifelse((session_number == n+1 & final_session), TRUE, FALSE),
      score = score_value,
      modality = session_modality
    ) %>%
    select(patient, date, session_number, modality, initial_session, final_session, score) %>%
    print()

}
adate %>% add_nsessions(7,"A",T)

lss <- list()

lss[[1]] <- "2020-01-02" %>% add_nsessions(
  7,  "A", F,
  c(20, 17, 19, 25, 21, 18, 15, 10),
  c("f2f","f2f","f2f","f2f","f2f","f2f","f2f","f2f")
)
lss[[2]] <- "2020-01-07" %>% add_nsessions(
  8,  "B",T,
  c(35, 38, 32, 34, 28, 25, 22, 25, 19),
  c("TMH","TMH","TMH","TMH","TMH","TMH","TMH","TMH","TMH")
)
lss[[3]] <- "2020-01-25" %>% add_nsessions(
  10, "C",T,
  c(45, 48, 42, 34,  38, 35, 32, 28,  33, 26, 22),
  c("TMH","TMH","TMH","TMH", "TMH","TMH","TMH","TMH", "TMH","TMH","TMH")
)
lss[[4]] <- "2020-02-23" %>% add_nsessions(
  9,  "D",F,
  c(50, 47, 42, 41,  46, 48, 48, 52,  51, 49),
  c("f2f","f2f","f2f","f2f","TMH","TMH","TMH","TMH","TMH", "TMH")
  )
lss[[5]] <- "2020-03-01" %>% add_nsessions(
  8,  "E",F,
  c(32, 35, 36, 37, 36, 39, 38, 42, 45),
  c("f2f","f2f","f2f","f2f","TMH","TMH","TMH","TMH","TMH")
)
lss[[6]] <- "2020-03-27" %>% add_nsessions(7,  "F",F, 30)
lss[[7]] <- "2020-04-04" %>% add_nsessions(9,  "G",F, 35)

ds_reprex <- lss %>% bind_rows()

g2 <-
  ds_reprex %>%
  ggplot(aes(
    x = date
    ,y = score

  ))+
  geom_line(aes(group = patient))+
  geom_point(aes(fill = modality), shape = 21, size = 3)+
  geom_point(
    data  = ds_reprex %>% filter(final_session == TRUE)
    ,size = 5, shape = 13
  )+
  geom_text(aes(label = patient),
    data = ds_reprex %>% filter(initial_session == T), nudge_x = -3
  )+
  geom_vline(xintercept = as_date("2020-03-11"), linetype = "dashed",alpha = .5)+
  geom_vline(xintercept = as_date("2020-03-23"), linetype = "dashed", alpha = .5)+
  # scale_x_continuous(breaks = seq(0,100, 50))+
  scale_fill_viridis_d(option = "magma")+
  labs(
    title = "Fictionalized trajectories"
    # ,y = "Cumulative Cases (in thousands)"
    # ,x = "Days since the first confirmed case outside of China"
    # ,caption = "(first dot) = 1st confirmed case, (second dot) = 1st confirmed death,
    # (dotted line) = pandemic announced by WHO, (dashed lines) = 75 and 100th day since Exodus"
  )
g2
# you can overwrite the mapping to plot a different measure:


# ---- publish ---------------------------------------
path_report <- "./analysis/covid-trajectory/covid-trajectory-1.Rmd"
rmarkdown::render(
  input = path_report,
  output_format=c("html_document")
  ,clean=TRUE
)


