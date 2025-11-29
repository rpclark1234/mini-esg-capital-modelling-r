generate_correlated_normals <- function(n_steps, n_scenarios, corr_matrix) {
  dim <- nrow(corr_matrix)
  L   <- chol(corr_matrix)  # upper triangular
  
  Z <- matrix(rnorm(n_steps * n_scenarios * dim),
              ncol = dim)
  
  Z_corr <- Z %*% L
  
  array(Z_corr, dim = c(n_steps, n_scenarios, dim))
}
