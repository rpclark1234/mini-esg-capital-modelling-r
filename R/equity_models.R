#Function to simulate the equity over given time periods and scenarios

simulate_equity <- function(S0, mu_S, sigma_S, dt, shocks) {
  n_steps     <- dim(shocks)[1]
  n_scenarios <- dim(shocks)[2]
  
  S <- matrix(0, nrow = n_steps + 1, ncol = n_scenarios)
  S[1, ] <- S0
  
  drift <- (mu_S - 0.5 * sigma_S^2) * dt
  
  for (t in 1:n_steps) {
    S[t + 1, ] <- S[t, ] * exp(
      drift + sigma_S * sqrt(dt) * shocks[t, ]
    )
  }
  
  S
}
