#' Creates a correlation table with associated standard errors
#'
#' Creates a correlation matrix with associated SEs for easy export.
#'
#' @param data Data frame.
#' @param digits Number of digits to print.
#' @param export Logical. If TRUE, the correlation table is exported as a csv file to the working directory. The default is FALSE.
#' @param sep The field separator string to be used if export = TRUE.
#' @return Correlation matrix with SEs.
#' @import psych
#' @export

corr_se <- function(data, digits = 2, export = FALSE, sep = ";"){
  
  r <- psych::corr.test(data) 
  r_se <- paste0(formatC(r$r, digits = 2, format = "f"), " (", formatC(r$se, digits = 2, format = "f"), ")")
  r_se <- gsub("0\\.","\\.", r_se)
  dim(r_se) <- c(nrow(r$r), ncol(r$r))
  r_se[upper.tri(r_se)] <- NA
  diag(r_se) <- NA
  colnames(r_se) <- rownames(r_se) <- colnames(r$r)
  
  if(export){
    
  write.table(r_se, "corr_se.csv", sep = ",", col.names = NA)
    
  } else {
    
  return(r_se)
    
  }
   

}
