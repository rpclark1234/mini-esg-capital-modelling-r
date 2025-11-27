library(tidyverse)
library(lubridate)

rates  <- read_csv("data/rates.csv")
equity <- read_csv("data/equity.csv")
cpi    <- read_csv("data/cpi.csv")

# Rename variables and transform date to a date object
rates <- rates %>% rename(Date = observation_date, Rate = IR3TIB01EZM156N) %>% mutate(Date = as.Date(Date))

# Transform the dates to a Date object and select the two relevant columns fom the dataset
equity <- equity %>% mutate(Date = mdy(Date)) %>% select(Date, Equity = Price)

# Transform the date by adding the day "01" to each entry and rename variables
cpi <- cpi %>% mutate(Category = ymd(paste0(Category, "-01"))) %>% rename(Date = Category) %>% rename(Cpi = 'Euro area')

# Join the three data frames according to their dates into one dataframe
data <- rates %>% inner_join(equity, by = "Date") %>% inner_join(cpi, by = "Date") %>% arrange(Date)

# Create 2 new variables that represent rate of increase/decrease of equity and Cpi compared to the previous year
data <- data %>% mutate(equity_ret = Equity/lag(Equity) - 1, inflation_rate = Cpi/lag(Cpi) -1) %>% drop_na()