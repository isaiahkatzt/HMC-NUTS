### Implementation of the Random Walk Metropolis Algorithm 

set.seed(24) 

# establish mean and covariance structure of target distribution 
d = 10
cov.generator = matrix(runif(d * d,0,1), nrow = d, ncol = d)
cov <- cov.generator %*% t(cov.generator)
mu = numeric(d)
  
target <- function(x) {
  dmvnorm(x, mean = mu, sigma = cov)
}

proposal <- function(x) { 
  return(rnorm(d, mean = x, sd = 1))
}

rw_metropolis <- function(target_dist, prop_dist, x = rep(0, d), burn = 1e4, iters = 1e5) {
  
  # output storage for later convergence analysis 
  samples <- matrix(numeric(iters * d), nrow = iters, ncol = d)

  for (i in 1:iters) { 
    
    # sample from proposed distribution
    z <- prop_dist(x)
    
    # calculate acceptance probability 
    alpha <- target_dist(z) / target_dist(x) 
    r <- min(1, alpha)
    
    #  determine acceptance / rejection 
    u <- runif(1)
    
    if (r > u) {
      x <- z
    }
    samples[i, ] <- x 
  }
  samples <- samples[-c(seq(1,burn))]
  return(samples)
}
