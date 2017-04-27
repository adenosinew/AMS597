#Function5
#Yuanjiao Wang

#' Weighted Z test combination
#'
#' @param x a (non-empty) numberic vector of data values.
#' @param y a (non-empty) numberic vector of data values.
#' @param alternative a character string indicates the alternative hypothesis and must be one of "two.sided", "greater" or "less".
#'
#' @return A list containing the following components:
#' @return \code{pc} the value of weighted z test combination statistic
#' @return \code{p.value} the p-value for the test
#'
#' @examples
#' x <- c(3,5,8,12,9,23,42,NA,58,NA,NA)
#' y <- c(10,5,NA,8,NA,NA,18,NA,45,66,44)
#' weighted.z.test(x,y,alternative="two.sided") #pc=1.234111  p.value=0.2171614

weighted.z.test <- function(x, y, alternative) {

  if (length(x)!=length(y)){
    print(t.test(x,y))
  }
  else{
  paired.x <- x[!is.na(x) == !is.na(y)]
  paired.y <- y[!is.na(x) == !is.na(y)]
  tumor <- x[!is.na(x) & is.na(y)]
  normal <- y[!is.na(y) & is.na(x)]
  n1 <- length(paired.x)
  n2 <- length(tumor)
  n3 <- length(normal)
  if (n1==1|n2==1|n3==1) {
    stop("not enough observations")
  }

  if(n1==0){
    print(t.test(x,y))
  }

  if(n2==0 & n3==0){
    print(t.test(x,y,paired = T))
  }

  else{
  w1 <- sqrt(n1)
  w2 <- sqrt(n2 + n3)
  t1 <- t.test(paired.x, paired.y, alternative, paired = TRUE)
  p1 <- t1$p.value
  t2 <- t.test(tumor, normal, alternative, paired = FALSE)
  p2 <- t2$p.value
  z1 <- qnorm(1 - p1)
  z2 <- qnorm(1 - p2)
  pc <- (w1 * z1 + w2 * z2) / sqrt((w1 ^ 2) + (w2 ^ 2))
  if (alternative == "less") {
    pv <- pnorm(pc)
  }
  if (alternative == "greater") {
    pv <- 1 - pnorm(pc)
  }
  if (alternative == "two.sided") {
    pv <- 2 * min(pnorm(pc), 1 - pnorm(pc))
  }
  return(list(statistic = pc, p.value = pv))
}}}
