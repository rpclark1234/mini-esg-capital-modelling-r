#Function to simulate the interest rates over given time periods and scenarios

simulate_rates <- function(r0, a, b, sigma_r, dt, shocks) {
  n_steps     <- dim(shocks)[1]
  n_scenarios <- dim(shocks)[2]
  
  r <- matrix(0, nrow = n_steps + 1, ncol = n_scenarios)
  r[1, ] <- r0
  
  for (t in 1:n_steps) {
    r[t + 1, ] <- r[t, ] +
      a * (b - r[t, ]) * dt +
      sigma_r * sqrt(dt) * shocks[t, ]
  }
  
  r
}
