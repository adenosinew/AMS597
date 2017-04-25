# Function 2 #
# Yicong Zhu #

#' corrected Z-test of Looney and Jones
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param y a(non-empty) numeric vector of data values.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or"less". You can specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in means if you are performing a two sample test).
#'
#' @return A list containing the following components
#' @param statistic the value of the corrected Z-test statistic.
#' @param p.value the p-value for the test.
#' @param method the method the function used.
#'
#' @examples
#' x <- c(3,2,5,13,54,63,22,11,NA,NA,NA)
#' y <- c(8,3,10,NA,NA,NA,NA,NA,43,96,14)
#' corrected_Z(x,y) # z.stat = -0.4384378  p.value = 0.6610690


corrected_Z<-function(x,y,alternative="two.sided",mu=0){
  paired.x<-x[!is.na(x)==!is.na(y)]
  paired.y<-y[!is.na(x)==!is.na(y)]
  tumor<-x[!is.na(x) & is.na(y)]
  normal<-y[is.na(x) & !is.na(y)]
  n1<-length(paired.x)
  n2<-length(tumor)
  n3<-length(normal)
  t0<-c(paired.x,tumor)
  n0<-c(paired.y,normal)
  if(n1==0){
    s<-0
    method="two-sample Z-test"
  }else{
    s<-cov(paired.x,paired.y)
    if (n2==0 & n3==0) method="paired sample Z-test"
    else method="corrected Z-test"
  }
  z.stat<-(mean(t0)-mean(n0)-mu)/sqrt(sd(t0)^2/(n1+n2)+sd(n0)^2/(n1+n3)-2*n1*s/(n1+n2)/(n1+n3))
  if(alternative=="two.sided"){
    p.value=2*pnorm(abs(z.stat),lower.tail=F)
  }
  if(alternative=="greater"){
    p.value=pnorm(z.stat,lower.tail=F)
  }
  if(alternative=="less"){
    p.value=pnorm(z.stat,lower.tail=T)
  }
  return(list(statistic=z.stat,p.value=p.value,METHOD=method))
  
}