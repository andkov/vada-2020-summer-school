# print names and associated lables of variables (if attr(.,"label)) is present
names_labels <- function(ds){
  dd <- as.data.frame(ds)

  nl <- data.frame(matrix(NA, nrow=ncol(dd), ncol=2))
  names(nl) <- c("name","label")
  for (i in seq_along(names(dd))){
    # i = 2
    nl[i,"name"] <- attr(dd[i], "names")
    if(is.null(attr(dd[[i]], "label")) ){
      nl[i,"label"] <- NA}else{
        nl[i,"label"] <- attr(dd[,i], "label")
      }
  }
  return(nl)
}
# names_labels(ds=oneFile)

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
# ds %>% distinct(id) %>% count() %>% neat(10)

# adds a formated datatable
neat_DT <- function(x, filter_="top",...){

  xt <- x %>%
    as.data.frame() %>%
    DT::datatable(
      class   = 'cell-border stripe'
      ,filter  = filter_
      ,options = list(
        pageLength = 6,
        autoWidth  = FALSE
      )
      , ...
    )
  return(xt)
}
# dt <- neat_DT # alias for quick reference

# function to create a look up tables for a given chapter of OECD data (e.g. Health Status, Health Resources)
get_var_unit_lookup <- function(list_object){
  # list_object <- ls_input_health$health_resources

  d_var_unit <-  list_object$data %>% dplyr::distinct(VAR,UNIT) %>% tibble::as_tibble()
  d_var_unit <- d_var_unit %>%
    dplyr::left_join(list_object$structure$VAR, by = c("VAR" = "id")) %>%
    dplyr::rename(var_label = label) %>%
    dplyr::left_join(list_object$structure$UNIT, by = c("UNIT" = "id")) %>%
    dplyr::rename(unit_label = label) %>%
    dplyr::arrange(VAR,UNIT)
  return(d_var_unit)
}
# How to use
# dvars_health_resources <- ls_input_health$health_resources %>% get_var_unit_lookup()
# dvars_health_status <- ls_input_health$health_status %>% get_var_unit_lookup()

# compute_rank <- function(list_object, var_name, unit_name, d_country = ds_country){
compute_rank <- function(list_object, var_label_i, unit_label_i, d_country = ds_country){
  # list_object <- ls_input_health$health_resources
  # var_name <- "HOPITBED"
  # unit_name     <- "RTOINPNB"
  # var_label_i <- "Practising caring personnel"
  # unit_label_i <- "Density per 1 000 population (head counts)"

  d_country <-
    readr::read_csv(
      # config$path_country
      "data-public/metadata/oecd/country.csv"
    ) %>%
    dplyr::filter(desired)

  var_unit <- list_object %>%
    get_var_unit_lookup() %>%
    # dplyr::filter(VAR == var_name, UNIT == unit_name)
    dplyr::filter(var_label == var_label_i, unit_label == unit_label_i)

  var_name <- var_unit %>% dplyr::pull(VAR)
  unit_name <- var_unit %>% dplyr::pull(UNIT)

  d_measure <- list_object$data %>%
    dplyr::filter(VAR == var_name, UNIT == unit_name ) %>%
    # dplyr::filter(var_label == var_label_i, unit_label == unit_label_i ) %>%
    dplyr::filter(COU %in% (d_country %>% dplyr::filter(desired) %>%  dplyr::pull(id)) ) %>%
    dplyr::group_by(COU) %>%
    dplyr::summarize(
      min_year = min(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
      ,max_year = max(obsTime,na.rm=T) # TODO: MUST MACH ONLY YEARS FOR WHICH MEASURE VALUE IS NA!!!
      ,mean = mean(obsValue, na.rm = T)
      ,median = median(obsValue, na.rm = T)
      ,value = sum(mean, median)/2

    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      rank_percentile = dplyr::percent_rank(value)
      ,rank = dplyr::dense_rank(value)
      ,n_tile = dplyr::ntile(value, 5)
    )
  var_unit <- dplyr::bind_rows(
    var_unit,
    tibble::as_tibble(  as.data.frame(matrix(nrow = nrow(d_measure)-1, ncol=ncol(var_unit))) )
  ) %>%
    dplyr::select(names(var_unit)) %>%
    tidyr::fill(names(var_unit))
  d_out <- var_unit %>% dplyr::bind_cols(d_measure)
  return(d_out)
}
# How to use
# d_measure <- ls_input_health$health_resources %>% compute_rank("HOPITBED","RTOINPNB")
# d_measure <- ls_input_health$health_resources %>% compute_rank("Practising caring personnel","Density per 1 000 population (head counts)")

compute_epi_timeline <- function(d, n_deaths_first_day = 1) { #}, d_country ){
  # browser()
  # d <- ds_covid
  n_deaths_first_day = 1

  d_country <-
    readr::read_csv(config$path_country) %>%
    dplyr::filter(desired)

  d_out <- d %>%
    dplyr::filter(country_code %in% unique(d_country$id)) %>%
    dplyr::group_by(country_code) %>%
    dplyr::mutate(
      # this solution might be vulnerable to cases where some intermediate dates are missed
      n_deaths_cum  = cumsum(n_deaths)
      ,cutoff       = n_deaths_cum >= n_deaths_first_day
      ,epi_timeline = cumsum(cutoff)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(epi_timeline > 0) %>%
    dplyr::mutate(
      time_since_exodus = date - lubridate::date("2020-01-13")
    )
  return(d_out)
}
# how to use
# d_covid <- ds_covid %>% compute_epi_timeline(n_deaths_first_day = 1)

