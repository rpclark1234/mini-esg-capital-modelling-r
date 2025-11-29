library(tidyverse)
library(lubridate)

source("R/utils.R")
source("R/interest_rate_models.R")
source("R/equity_models.R")
source("R/inflation_models.R")

rates  <- read_csv("data/rates.csv")
equity <- read_csv("data/equity.csv")
cpi    <- read_csv("data/cpi.csv")

# Rename variables and transform date to a date object
rates <- rates %>% rename(Date = observation_date, Rate = IR3TIB01EZM156N) %>% mutate(Date = as.Date(Date))

# Transform the dates to a Date object and select the two relevant columns fom the dataset
equity <- equity %>% mutate(Date = mdy(Date)) %>% select(Date, Equity = Price)

# Transform the date by adding the day "01" to each entry and rename variables
cpi <- cpi %>% mutate(Category = ymd(paste0(Category, "-01"))) %>% rename(Date = Category) %>% rename(Cpi = 'Euro area') %>% drop_na() %>% filter(Cpi != 0)

# Join the three data frames according to their dates into one dataframe
data <- rates %>% inner_join(equity, by = "Date") %>% inner_join(cpi, by = "Date") %>% arrange(Date)

# Create 2 new variables that represent rate of increase/decrease of equity and Cpi compared to the previous year
data <- data %>% mutate(equity_ret = Equity/lag(Equity) - 1, inflation_rate = Cpi/lag(Cpi) -1) %>% drop_na()

#########################################
# CREATE BASIC MODELS FOR EACH VARIABLE AND THEN FIND A MATRIX OF THE CORRELATION COEFFICIENTS BETWEEN THE
# RESIDUALS OF EACH MODEL. THIS WILL DETERMINE IF VARIABLES REACT TO SHOCKS IN THE SAME WAY.

dt <- 1/12  # one month in years

#INTEREST RATE

r <- data$Rate
r_lag <- head(r, -1)
r_next <- tail(r, -1)

r_reg <- lm(r_next ~ r_lag)
summary(r_reg)

# Estimates of rate regression parameters
c_hat <- coef(r_reg)[1]
phi_hat <- coef(r_reg)[2]
sigma_r_hat <- sd(residuals(r_reg))

#Conversion to Vasicek parameters
a       <- -log(phi_hat) / dt
b       <- c_hat / (1 - phi_hat)
sigma_r <- sigma_r_hat  

#EQUITY
ret <- data$equity_ret
mu_hat <- mean(ret)
sigma_e_hat <- sd(ret)

mu_S <- mu_hat
sigma_S <- sigma_e_hat

#INFLATION

pi <- data$inflation_rate
pi_lag <- head(pi, -1)
pi_next <- tail(pi, -1)

pi_reg <- lm(pi_next ~ pi_lag)
coef(pi_reg)

alpha_hat <- coef(pi_reg)[1]
beta_hat <- coef(pi_reg)[2]
sigma_pi <- sd(residuals(pi_reg))

# CREATE CORRELATION MATRIX

resid_r <- residuals(r_reg)
resid_eq <- ret - mu_S
resid_pi <- residuals(pi_reg)
n <- min(length(resid_r), length(resid_eq), length(resid_pi))
         
resid_matrix <- cbind(resid_r[1:n], resid_eq[1:n], resid_pi[1:n])

cor_mat <- cor(resid_matrix)

#################################################################################
#IN THIS SECTION, THE 10,000 SIMULATIONS ARE RUN OVER 10 YEARS.

T_years <- 10
n_steps <- as.integer(T_years/dt)
n_scenarios <- 10000

shocks_array <- generate_correlated_normals(n_steps = n_steps, n_scenarios = n_scenarios, corr_matrix = cor_mat)

eps_r <- shocks_array[ , , 1]
eps_eq <- shocks_array[ , , 2]
eps_pi <- shocks_array[ , , 3]

r0 <- tail(data$Rate, 1)
eq0 <- tail(data$Equity, 1)
pi0 <- tail(data$inflation_rate, 1)

r_paths <- simulate_rates(r0, a, b, sigma_r, dt, eps_r)
eq_paths <- simulate_equity(eq0, mu_S, sigma_S, dt, eps_eq)
pi_paths <- simulate_inflation(pi0, alpha_hat, beta_hat, sigma_pi, dt, eps_pi)





