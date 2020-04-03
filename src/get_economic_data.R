# Download Economic Indicators From FRED API --------------------

library(fredr)

# my_fred_key <- '5fb47a42104bd917db0e26c389******'

# fredr_set_key(my_fred_key)

# Let's get all available indicators for China in FRED database.

china <- fredr_request(endpoint = "tags/series", tag_names = "china")

china %>%
  select(
    popularity, id, title, frequency_short, units_short, seasonal_adjustment_short,
  ) %>%
  dplyr::filter(.data=., 
                frequency_short %in% c("D", "M"), 
                !str_detect(title, 'DISCONTINUED'),
                seasonal_adjustment_short !="SA") %>%
  arrange(desc(popularity)) %>%
  view()

econ_indicators <- c(ex_rate_daily = 'DEXCHUS', 
                     ex_rate_monthly = 'EXCHUS', 
                     realeff_ex_rate = 'CCRETT01CNM661N',
                     exports = 'XTEXVA01CNM667S', 
                     cpi = 'CHNCPIALLMINMEI', 
                     interest_rates = 'INTDSRCNM193N',
                     confidence_index = 'CSCICP03CNM665S', 
                     un_rate = 'LMUNRRTTCNQ156S',
                     normalized_gdp = 'CHNLORSGPNOSTSAM')

### 1. GDP

# Leading Indicators OECD: Reference series: Gross Domestic Product (GDP):
# Normalised for China (CHNLORSGPNOSTSAM)
# [CHNLORSGPNOSTSAM](https://fred.stlouisfed.org/series/CHNLORSGPNOSTSAM)

normalized_gdp <- econ_indicators[['normalized_gdp']]

gdp_china <- fredr(
  series_id = normalized_gdp,
  observation_start = as.Date("2019-01-01"),
  observation_end = as.Date("2020-03-15"),
  frequency = "m"
)

plot(gdp_china$date, gdp_china$value,
  type = "b",
  xlab = "Time", ylab = "GDP"
)

## 2. prices and inflation

# Consumer Price Index for China



# Real Effective Exchange Rates Based on Manufacturing Consumer Price Index for China


## 3. Labor market variables.

# Registered Unemployment Rate for China, Seasonally Adjusted, Quarterly.


# saveRDS(china, 'data/Econ_China.Rds')


library(Quandl)
NASDAQ_CNY <- Quandl("NASDAQOMX/NQCN2000CNY", api_key="jsMbTodosyHDq3sWMuzo")

EERI_mon_real <-Quandl("BIS/EM_MRNTW", api_key="jsMbTodosyHDq3sWMuzo") #Weighted averages of bilateral exchange rates, 
#where the weights are based on manufacturing trade flows and capture direct bilateral trade as well as third-market competition.

Imp_Exp_Price_Ind <- Quandl("BLSN/EIUCOCHNTOT", api_key="jsMbTodosyHDq3sWMuzo")
#Series: China (Dec. 2003=100) - All commodities
#Index Type: LOCALITY OF ORIGIN
#Not Seasonally Adjusted
#Additional references: BLS's Import/Export Price Indexes Overview page