############################ ####
############################ ####
#### DATA CLEANING FUNCTIONS ####
############################ ####
############################ ####
# Last Change: 04.04.2018 ####
#### Content: ####
#1: Pattern Repetition Detection ####
#2: Outlier exclusion ####
############################################################################################ ####
################################################## ####
#### 1a) Logical for Certain Response Pattern Size ####
################################################## ####
# Author: Mattis Geiger
#### function to generate vector of response pattern variance and identify cases of repeated patterns
#### Comment (Mattis, 26.03.2018): for could be changed to apply to make function run faster.
#### Comment (Mattis, 26.03.2018): could add the function to ignore non-numeric columns.
pattfixRep <- function(data,firstitem,itemnumber,patternsize)
  {
  classmat <- matrix(,nrow=nrow(data),ncol=ncol(data[,firstitem:(firstitem+(itemnumber-1))])%/%patternsize)
  for(j in 1:(ncol(data[,firstitem:(firstitem+(itemnumber-1))])%/%patternsize)) classmat[,j] <- 
      {
    rowSums(t(t(data[,(firstitem+patternsize*j-patternsize):(firstitem+patternsize*j-1)])*10^(patternsize-1:patternsize)))
        }
  repVar <- apply(classmat,1,var)
  which(repVar==0)
}


######################################################## ####
#### 1b) Report Row Number for Any Response Pattern Size ####
######################################################## ####
# Author: Mattis Geiger
#### function to run any patternsize 2:(itemnumber/2)
#### Comment (Mattis, 26.03.2018): for could be changed to apply to make function run faster.
#### Comment (Mattis, 26.03.2018): could add the function to ignore non-numeric columns.
pattRep <- function(data,firstitem,itemnumber)
  {
  repVarMat <- matrix(,nrow=nrow(data),ncol=length(2:(itemnumber/2)))
  for(k in 2:(itemnumber/2)) repVarMat[,(k-1)] <- 
      {
    classmat <- matrix(,nrow=nrow(data),ncol=ncol(data[,firstitem:(firstitem+(itemnumber-1))])%/%k)
    for(j in 1:(ncol(data[,firstitem:(firstitem+(itemnumber-1))])%/%k)) classmat[,j] <- 
        {
      rowSums(t(t(data[,(firstitem+k*j-k):(firstitem+k*j-1)])*10^(k-1:k)))
          }
    apply(classmat,1,var)
    }
  repVarMat
  repCase <- which(repVarMat==0,arr.ind=TRUE)
  unique(repCase[,1])
}
#### Arguments:
# data: data.frame to be analyzed. 
# firstitem: col.number of the first item of the 
# itenumber: number of items the questionnaire(s) consist(s) of
#### Note: items of the questionnaire that is meant to be checked must be presented consecutively and in the order as they were presented to the participant.
#### Note: items of the questionnaire must be in raw format, i.e. before recoding items or conducting other transformations and scorings.

############################################################################################ ####
######################################################## ####
#### 2a) iterative univariate outlier exclusion function ####
######################################################## ####
# Author: Mattis Geiger; special thanks to: Luc Watrin
# features:
# runs on data frames only! transform regular matrices to data.frames before running!
# ignores non-numeric columns of the data.frame
# iteratively removes all outliers > SD.threshold per column by replacing them with NA
it.univ.out <- function(data,SD.threshold) {
  sapply(data, function(x) if(is.numeric(x)){
    if(max(abs(scale(x)), na.rm = TRUE) < SD.threshold)
    {
      return(x)
    }
    else
    {
      while(max(abs(scale(x)), na.rm = TRUE) > SD.threshold)
      {
        x[abs(scale(x)) == max(abs(scale(x)), na.rm = TRUE)] <- NA
      }
      return(x)
    }
  }
  else{return(x)}
  )
}

########################################################## ####
#### 2b) iterative multivariate outlier exclusion function ####
########################################################## ####
# Author: Mattis Geiger
# features:
# employs outlier()-command from the psych package
# iteratively excludes multivariate outliers based on Mahalanobis Distance and the related chi^2-distribution
# cut-off quantile can be chosen 
#### Comment (Mattis, 04.04.2018): currently only runs on data.frames. extend to matrix format.
it.multiv.out <- function(data,quantile)
{
  only.numeric <- 
    while(max(outlier(data[,sapply(data, is.numeric)],plot=FALSE))>qchisq(quantile,ncol(data[,sapply(data, is.numeric)])))
    {
      data <- data[-which.max(outlier(data[,sapply(data, is.numeric)],plot=FALSE)),]
    }
  return(data)
}
