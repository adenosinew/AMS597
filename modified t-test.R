#' Modified t-test
#'
#' @param x value of the "tumor"
#' @param y value of the "normal"
#' @param alternative
#' @param mu
#'
#' @return \code{statistic} value of statistic
#' @return \code{p.value} P value
#' @export
#'
#' @examples
#' x <- c(1,2,3,4,NA,NA,1)
#' y <- c(1,2,5)
#' my.mod.t.test(x,y)
my.mod.t.test = function (x,y,alternative="two.sided",mu=0){

  if (length(x)!=length(y)){
    t <- t.test(x,y)
    t3 <- t$statistic
    p.value <- t$p.value
  }
  else {
 paired.x <-x[!is.na(x)==!is.na(y)]
 paired.y <-y[!is.na(y)==!is.na(x)]
 D <- paired.x- paired.y
 tumor<-x[!is.na(x) & is.na(y)]
 normal<-y[is.na(x) & !is.na(y)]
 Dbar <- mean(D[!is.na(D)])
 tumorbar <- mean(tumor[!is.na(tumor)])
 normalbar <- mean(normal[!is.na(normal)])

 n1=length(paired.x)
 n2=length(tumor)
 n3=length(normal)
 nh=2*n2*n3/(n2+n3)

 if (n1==1|n2==1|n3==1) {
   stop("not enough observations")
 }
 else {
 VD <- var(D[!is.na(D)])
 VT <- var(tumor[!is.na(tumor)])
 VN <- var(normal[!is.na(normal)])


 if (n1!=0 & n2!=0 & n3!=0)
 { t3 <- (n1*Dbar+nh*(tumorbar-normalbar))/sqrt(n1*VD+nh^2*(VN/n3+VT/n2))
 if(alternative=="two.sided"){
   p.value=2*pnorm(abs(t3),lower.tail=F)
 }
 if(alternative=="greater"){
   p.value=pnorm(t3,lower.tail=F)
 }
 if(alternative=="less"){
   p.value=pnorm(t3,lower.tail=T)
 }
 }

  if (n2==0 | n3 == 0 & n1!=0) {
   t <- t.test(D)
   t3 <- t$statistic
   p.value <- t$p.value
 }
  if (n1==0){
    t <- t.test(x,y)
    t3 <- t$statistic
    p.value <- t$p.value
  }
 }}
  return(list(statistic=t3,p.value=p.value))
  }
