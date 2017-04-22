#' MLE method procedure under homo- or heteroscedasticity
#'
#' @param data Data Frame
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. If TRUE then the process homo- MLE otherwise heteroscdasticity case is used.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#'
#' @return
#' @export
#'
#' @examples
#' mle(x)
#' mle(x, var.equal = F, alternative = less)
mle <- function(data, var.equal = TRUE, alternative=c("two.sided","less","greater")){
  # Prepare the data
  source(dataPrep(data))

  # For MLE methods
  Tbar <- mean(vecn2)
  Nbar <- mean(vecn3)

  Tbar1 <- mean(n1.matrix[,1])
  Nbar1 <- mean(n1.matrix[,2])

  ST <- sd(vecn2)
  SN <- sd(vecn3)

  ST1 <- sd(n1.matrix[,1])
  SN1 <- sd(n1.matrix[,2])
  STN1 <- cov(n1.matrix)

  r <- STN1/(ST1*SN1)

  # For homo- case
  if (var.equal==TRUE){
    METHOD <- "MLE based test of Ekbohm under homoscedasticity"
    sigma2t <- (ST1^2)*(n1-1)
    sigma2n <- (SN1^2)*(n1-1)
    sigma2tn <- (1+r^2)*((ST^2)(n2-1)+(SN^2)(n3-1))
    sigma2 <- (sigma2t+sigma2n+sigma2tn)/(2*(n1-1)+(1+r^2)*(n2+n3-2))
    V1 <- sigma2*((2*n1*(1-r)+(n2+n3)*(1-r^2))/((n1+n2)*(n1+n3)-n2*n3*(r^2)))
    f <- n1*(n1+n3+n2*r)*(((n1+n2)*(n1+n3)-n2*n3*(r^2))^(-1))
    g <- n1*(n1+n2+n3*r)*(((n1+n2)*(n1+n3)-n2*n3*(r^2))^(-1))
    zmle <- (f*(Tbar1-Tbar)-g*(Nbar1-Nbar)+Tbar-Nbar)/sqrt(V1)
  }

  # For heteroscedasticity case
  if (var.equal==FALSE){
    METHOD <- "MLE based test of Lin and Stivers under heteroscedasticity"
    f <- n1*(n1+n3+n2*STN1/(ST1^2))*(((n1+n2)*(n1+n3)-n2*n3*(r^2))^(-1))
    g <- n1*(n1+n2+n3*STN1/(SN1^2))*(((n1+n2)*(n1+n3)-n2*n3*(r^2))^(-1))
    vt <- (((f^2)/n1)+((1-f)^2)/n2)*(ST1^2)*(n1-1)
    vn <- (((g^2)/n1)+((1-g)^2)/n3)*(SN1^2)*(n1-1)
    vtn <- f*g*STN1*(n1-1)/n1
    V1 <- (vt+vn-2*vtn)/(n1-1)
    zmle <- (f*(Tbar1-Tbar)-g*(Nbar1-Nbar)+Tbar-Nbar)/sqrt(V1)
  }

  # Calculate p value
  pv <- pval(zmle, alternative)

  # Output
  output(zmle, pv, METHOD)
}
