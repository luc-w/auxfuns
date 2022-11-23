#v0.3
# function to compute corrected item total correlations of dichotomous items
rit_bis <- function(dat){
  rit.bis <- NULL
  dat <- as.matrix(dat)
  for(i in 1:ncol(dat)){
    rit.bis[i] <- wCorr::weightedCorr(rowSums(dat[, -i]),
                               dat[, i], 
                               method = c("Polyserial"))
    }
  names(rit.bis) <- colnames(dat)
  return(rit.bis)
}