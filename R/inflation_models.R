#Function to simulate the inflation over given time periods and scenarios

simulate_inflation <- function(pi0, alpha, beta, sigma_pi, dt, shocks) {
  n_steps     <- dim(shocks)[1]
  n_scenarios <- dim(shocks)[2]
  
  pi <- matrix(0, nrow = n_steps + 1, ncol = n_scenarios)
  pi[1, ] <- pi0
  
  for (t in 1:n_steps) {
    pi[t + 1, ] <- alpha +
      beta * pi[t, ] +
      sigma_pi * sqrt(dt) * shocks[t, ]
  }
  
  pi
}
