#' Creates a correlation table with associated confidence intervals
#'
#' Creates a correlation matrix with associated bootstrapped 95% confidence intervals for easy export to Excel.
#' Based on psych::cor.ci().
#'
#' @param data Data frame.
#' @param n.iter Number of iterations to bootstrap over.
#' @param digits Number of digits to print.
#' @param export Logical. If TRUE, the correlation table is exported as a csv file to the working directory. The default is FALSE.
#' @param sep The field separator string to be used if export = TRUE.
#' @return Correlation matrix with bootstrapped 95% confidence intervals.
#' @import psych
#' @export
corr_ci <- function(data, n.iter = 10000, digits = 2, export = FALSE, sep = ";"){

  r <- psych::cor.ci(data, n.iter = n.iter, plot = F)

  r_est <- formatC(r$rho, digits = digits, format = "f")

  ci_lower <- ci_upper <- diag(nrow(r$rho))
  lower_triangle <- lower.tri(ci_lower, diag = F)

  ci_low_est <- formatC(r$ci$low.e, digits = digits, format = "f")
  ci_lower[lower_triangle] <- ci_low_est

  ci_up_est <- formatC(r$ci$up.e, digits = digits, format = "f")
  ci_upper[lower_triangle] <- ci_up_est

  r_ci <- paste0(r_est, " [", ci_lower, ",", ci_upper, "]")
  dim(r_ci) <- c(nrow(r$rho), ncol(r$rho))
  r_ci[upper.tri(r_ci)] <- NA
  diag(r_ci) <- NA
  colnames(r_ci) <- rownames(r_ci) <- colnames(r$rho)

  if(export){

  write.table(r_ci, "corr_ci.csv", sep = ",", col.names = NA)
  return(r_ci)

  } else {

  return(r_ci)

  }
}
