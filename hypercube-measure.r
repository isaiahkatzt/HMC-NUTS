# annulus calculation functions 
annulus_dist <- function(d_point) { 
  inverses <- rep(1, length(d_point)) - d_point
  return(min(min(inverses), min(d_point)))
}
annulus <- function(r, mins_list) {
  minslist[minslist <= r]
}

# meridian calculation functions 
equator_dist <- function(d_point) {
  dotprod <- rep(1, length(d_point)) %*% d_point
  return(abs(dotprod/length(d) - 0.5))
  }
meridian <- function(r, equator_list) {
  equator_list[equator_list <= r]
  }

# intersection calculation function 
intersection <- function(r, annulus_equator_dists) {
  int_vals <- annulus_equator_dists %>% 
    filter(annulus_dists <= r, equator_dists <= r)
  return(int_vals)
  }

# Simulation functions 
simulate_x <- function(d, iters) {
  points_matrix <- matrix(runif(d*iters), nrow = iters, ncol = d)
  annulus_dists <- apply(points_matrix, MARGIN = 1, FUN = annulus_dist)
  equator_dists <- apply(points_matrix, MARGIN = 1, FUN = equator_dist)
  return(data.frame(annulus_dists, equator_dists))
}

annulus_volume_est <- function(r, annulus_dists) {
  annulus_points <- annulus(r, annulus_dists)
  annulus_vol <- length(annulus_points) / length(annulus_dists)
  return(annulus_vol)
}

meridian_vol_est <- function(r, equator_dists) { 
  meridian_points <- meridian(r, equator_dists)
  meridian_vol <- length(meridian_points) / length(equator_dists)
  return(meridian_vol)
}

intersect_vol_est <- function(r, dists) {
  intersect_points <- intersection(r, dists)
  intersect_vol <- length(intersect_points) / length(dists)
}
