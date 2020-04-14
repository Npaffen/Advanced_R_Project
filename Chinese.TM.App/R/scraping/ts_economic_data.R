# helper function --------------------------------------------------
# function to standardize a time-series to a specific start value
normalize_to_x <- function(dataset, x) {
  factor_val <- dataset[[1]] - x
  normalize <- dataset - factor_val
}


# define the main function  --------------------------------------------------------


ts_economic_data <- function(start_date, end_date, econ_data) {


  ##### dowload economic data, adjust time span, standardize
  current_key <- Sys.getenv("FRED_API_KEY")
  fredr_set_key("c66bdbc4919216612f5cb63ec4994a81")

  if (econ_data == "dollar_yuan_exch") {
    dollar_yuan_exch <- fredr(
      series_id = "DEXCHUS",
      observation_start = start_date,
      observation_end = end_date,
      frequency = "d"
    ) %>%
      mutate(value = ifelse(is.na(value), value[is.na(value) + 1], value)) %>%
      # there is a missing value at 1st of Jan 2019, correct it by setting it to the value of 2nd Jan
      select(date, value)
  } else if (econ_data == "NASDAQ_CNY") {
    #
    NASDAQ_CNY <- Quandl("NASDAQOMX/NQCN2000CNY",
      api_key = "jsMbTodosyHDq3sWMuzo",
      transform = "normalize",
      order = "asc",
      collapse = "daily"
    ) %>%
      mutate(date = `Trade Date`, NASDAQ_Value = `Index Value`) %>%
      arrange(date) %>%
      dplyr::filter(between(date, start_date, end_date)) %>%
      mutate(NASDAQ_norm = normalize_to_x(NASDAQ_Value, 100)) %>%
      select(date, NASDAQ_norm)
  }
}
