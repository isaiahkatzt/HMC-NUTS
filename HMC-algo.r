leapfrog <- function(theta, r, epsilon, theta) {
  r_update <- r + (epsilon / 2) * grad(L*theta)
  theta_new <- theta + (epsilon * r_update)
  r_new <- r_update + (epsilon / 2) * grad(L*theta_new)
}

hamiltonian_mc <- function(theta_0, epsilon, L, scriptL, M) { 
  }