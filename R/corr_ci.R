corr_ci <- function(data, digits = 2){
  
  r <- psych::cor.ci(data, plot = F)
  
  r_est <- formatC(r$rho, digits = 2, format = "f")
  
  ci_lower <- ci_upper <- diag(nrow(r$rho))
  lower_triangle <- lower.tri(ci_lower, diag = F)
  
  ci_low_est <- formatC(r$ci$low.e, digits = 2, format = "f")
  ci_lower[lower_triangle] <- ci_low_est
  
  ci_up_est <- formatC(r$ci$up.e, digits = 2, format = "f")
  ci_upper[lower_triangle] <- ci_up_est
  
  r_ci <- paste0(r_est, " [", ci_lower, ",", ci_upper, "]")
  dim(r_ci) <- c(nrow(r$rho), ncol(r$rho))
  r_ci[upper.tri(r_ci)] <- NA
  diag(r_ci) <- NA
  colnames(r_ci) <- rownames(r_ci) <- colnames(r$rho)
  return(r_ci) 

}