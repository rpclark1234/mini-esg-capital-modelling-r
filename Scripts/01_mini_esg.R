library(tidyverse)
library(lubridate)
library(ggplot2)

source("R/utils.R")
source("R/interest_rate_models.R")
source("R/equity_models.R")
source("R/inflation_models.R")

rates  <- read_csv("data/rates.csv")
equity <- read_csv("data/equity.csv")
cpi    <- read_csv("data/cpi.csv")

# Rename variables and transform date to a date object
rates <- rates %>% rename(Date = observation_date, Rate = IR3TIB01EZM156N) %>% mutate(Date = as.Date(Date), Rate = Rate/100)

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


################################################################################

#COMPUTE THE DISCOUNT FACTORS FOR THE INTEREST RATES

# Drop the first row from rates as this was taken from the historical data
r_no0 <- r_paths[-1, , drop = FALSE]

# r_int is the integral of the rates up to time t. As the graph behaves as a
# step function, each running total- cumsum- can be multiplied by dt
r_int <- apply(r_no0, 2, cumsum) * dt   
DF    <- exp(-r_int)                    

# average the discounct factors- DF- for each time step 
time_grid <- (1:n_steps) * dt
df_mean   <- rowMeans(DF)

df_df <- tibble(
  t       = time_grid,
  df_mean = df_mean
)

################################################################################

#CREATE PLOTS OF DATA

# A PLOT OF THE FIRST 50 RATE PATHS

n_show <- 50  # number of scenarios to show
idx    <- sample(1:n_scenarios, n_show)

rates_plot_df <- tibble(
  t      = rep(0:n_steps * dt, times = n_show),
  rate   = as.vector(r_paths[, idx]),
  scen   = rep(paste0("scen_", 1:n_show), each = n_steps + 1)
)

ggplot(rates_plot_df, aes(x = t, y = rate, group = scen)) +
  geom_line(alpha = 0.3) +
  labs(title = "Sample simulated short-rate paths",
       x = "Time (years)", y = "Short rate") +
scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))


# A histogram of rate paths at 10 years

rates_10y <- r_paths[n_steps + 1, ]

tibble(r = rates_10y) %>%
  ggplot(aes(x = r)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribution of short rate at 10 years") +
scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_minimal()


# Equity Paths

equity_plot_df <- tibble(
  t    = rep(0:n_steps * dt, times = n_show),
  S    = as.vector(eq_paths[, idx]),
  scen = rep(paste0("scen_", 1:n_show), each = n_steps + 1)
)

ggplot(equity_plot_df, aes(x = t, y = S, group = scen)) +
  geom_line(alpha = 0.3) +
  labs(title = "Sample simulated equity index paths",
       x = "Time (years)", y = "Index level")

#DF Curve

ggplot(df_df, aes(x = t, y = df_mean)) +
  geom_line() +
  labs(title = "Average discount factor curve",
       x = "Time (years)", y = "DF(0,t)")




