ts_economic_data <- function(start_date, end_date, econ_data){
  library(Quandl)
  library(fredr)
  #### function to convert monthly econ data to daily econ data
  monthly_to_daily  <- function(ts_monthly) {
    df.xts <- xts(ts_monthly$Value,order.by = ts_monthly$Date)
    ts_monthly_daily <- na.locf(merge(df.xts, 
                                      foo=zoo(NA,
                                              order.by=seq(start(df.xts),
                                                           end(df.xts),
                                                           "day",
                                                           drop=F)))[, 1]) %>%
      tk_tbl() %>%
      
      rename("date" = "index", "Value" = "df.xts" )
  }
  
  #### function to standardize a time-series to a specific start value
  normalization_to_x <- function(dataset, x){
    factor_val <- dataset[1] - x
    normalize <- dataset - factor_val
  }
  
  
  
  ##### dowload economic data, adjust time span, standardize
  current_key <- Sys.getenv("FRED_API_KEY")
  fredr_set_key("c66bdbc4919216612f5cb63ec4994a81")
  
  
  if(econ_data == "dollar_yuan_exch") {
    dollar_yuan_exch <- fredr(
      series_id = "DEXCHUS",
      observation_start = start_date,
      observation_end = end_date,
      frequency = "d") %>%
      mutate(value = ifelse(is.na(value) == T,
                            value[is.na(value)+1],
                            value)) %>% 
      #there is a missing value at 1st of Jan 2019, correct it by setting it to the value of 2nd Jan
      select(date, value)
  } else if (econ_data == "NASDAQ_CNY"){
    #
    NASDAQ_CNY <- Quandl("NASDAQOMX/NQCN2000CNY",
                         api_key="jsMbTodosyHDq3sWMuzo",
                         transform = "normalize",
                         order = "asc",
                         collapse = "daily") %>%
      mutate("date" = .$`Trade Date`,
             "NASDAQ_Value" = .$`Index Value`) %>% 
      arrange(date) %>%
      filter(between(.$date,
                     start_date,
                     end_date)) %>%
      mutate("NASDAQ_norm" = normalization_to_x(.$NASDAQ_Value, 100)) %>%
      select(date, NASDAQ_norm )
  } 
}