
# Mini Economic Scenario Generator (R)

## Overview
- ESG built in R to simulate 3 variables over a 10 yer horizon:
      1) interest rates 
      2) equity index 
      3) inflation
- Calibrated to historical market data sourced from
      https://fred.stlouisfed.org/series/IR3TIB01EZM156N
      https://www.investing.com/indices/eu-stoxx50-historical-data
      https://www.ecb.europa.eu/stats/macroeconomic_and_sectoral/hicp/html/index.en.html?utm_source=chatgpt.com
- Intended as a simplified Internal Model / capital-modelling tool.

## Models
- Short rate: Vasicek-style (estimated via AR(1) regression).
- Equity: Geometric Brownian Motion on monthly returns.
- Inflation: AR(1) on monthly inflation rates.
- Joint simulation using Cholesky decomposition and a 3×3 correlation matrix of shocks.

## Data
- Monthly time series:
  - Short-term interest rate
  - Equity index level
  - CPI (price index)
- Stored in `data/rates.csv`, `data/equity.csv`, `data/cpi.csv`.

## Calibration
- AR(1) regressions using `lm()` to estimate mean reversion, long-term mean and volatility.
- Equity drift and volatility from empirical returns.
- Correlations from residuals of AR(1) and equity return series.

## Simulation
- Horizon: 10 years, monthly time steps (dt = 1/12).
- Number of scenarios: 10,000.
- Simulation functions defined in `R/`:
  - `simulate_rates()`
  - `simulate_equity()`
  - `simulate_inflation()`
  - `generate_correlated_normals()`

## Outputs
- Scenario paths for interest rates, equity and inflation.
- Scenario-based discount factors and average DF curve.
- Diagnostic plots of distributions and sample paths.

## How to run
1. Open the RStudio project.
2. Ensure packages `tidyverse`, `lubridate`, `ggplot2` are installed.
3. Place appropriate CSVs in the `data/` folder.
4. Download utils.R, interest_rate_models.R, inflation_models.R and equity_models.R
5. Run `scripts/01_mini_esg.R`.
