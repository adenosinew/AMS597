##function 3
##Xining Zhang


#' MLE based test of Ekbohm under homoscedasticity
#'
#' @param x vector value of "tumor"
#' @param y vector value of "normal"
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or"less". You can specify just the initial letter.
#'
#' @return A list containing the following components
#' @return \code{statistic} the value of the corrected Z-test statistic.
#' @return \code{p.value} the p-value for the test.
#'
#' @export
#'
#' @examples
#' homeMLE(x,y,alternative = "two.sided")
homoMLE <- function(x, y, alternative="two.sided") {

  if (length(x)!=length(y)){
    warning("tumor sample and normal sample have different dimension. Reduce to two sample t test")
    Ttest <- t.test(x,y)
    return(list(statistic <- Ttest$statistic,p.value <- Ttest$p.value, METHOD <- "Two sample T test"))
  }
  else{
    paired.x<-x[!is.na(x)==!is.na(y)]
    paired.y<-y[!is.na(x)==!is.na(y)]
    tumor<-x[!is.na(x) & is.na(y)]
    normal<-y[is.na(x) & !is.na(y)]
    n1<-length(paired.x)
    n2<-length(tumor)
    n3<-length(normal)

    if (n1==1|n2==1|n3==1) {
      stop("not enough observations")
    }

    if(n1==0){
      warning("No paired sample found. Reduce to two sample t test")
      Ttest <- t.test(x,y)
      return(list(statistic <- Ttest$statistic,p.value <- Ttest$p.value, METHOD <- "Two sample T test"))
    }

    if(n2==0 & n3==0){
      warning("No unpaired sample found. Reduce to paired t test")
      Ttest <- t.test(x,y,paired = TRUE)
      return(list(statistic <- Ttest$statistic,p.value <- Ttest$p.value, METHOD <- "Two sample T test"))
    }
    else{
      method <- "MLE based test of Ekbohm under homoscedasticity"
    T_bar <- mean(tumor)
    N_bar <- mean(normal)
    sd_T <- sd(tumor)
    sd_N <- sd(normal)
    T1_bar <- mean(paired.x)
    N1_bar <- mean(paired.y)
    sd_T1 <- sd(paired.x)
    sd_N1 <- sd(paired.y)
    sd_TN1 <- cov(paired.x, paired.y)

    r <- sd_TN1 / (sd_N1 * sd_T1)

    fstar <- n1 * (n1 + n3 + n2 * r) * ((n1 + n2) * (n1 + n3) - n2 * n3 * r ^
                                          2) ^ (-1)
    gstar <- n1 * (n1 + n2 + n3 * r) * ((n1 + n2) * (n1 + n3) - n2 * n3 * r ^
                                          2) ^ (-1)

    sigma2 <-
      (sd_T1 ^ 2 * (n1  - 1) + sd_N1 ^ 2 * (n1 - 1) + (1 + r ^ 2) * (sd_T ^ 2 *
                                                                       (n2 - 1) + sd_N ^ 2 * (n3 - 1))) / (2 * (n1 - 1) + (1 + r ^ 2) * (n2 + n3 - 2))
    V1_star <-
      sigma2 * ((2 * n1 * (1 - r) + (n2 + n3) * (1 - r ^ 2)) / ((n1 + n2) * (n1 + n3) - n2 *
                                                                  n3 * r ^ 2))


    Z.star = (fstar * (T1_bar - T_bar) - gstar * (N1_bar - N_bar) + T_bar -
                  N_bar) / sqrt(V1_star)
    if (alternative == "greater") {
      P.value = pt(Z.star, n1, lower.tail = F)
    }
    else if (alternative == "less") {
      P.value = pt(Z.star, n1, lower.tail = T)
    }
    else if (alternative == "two.sided") {
      P.value = 2 * pt(abs(Z.star), n1, lower.tail = F)
    }
    else{
      stop("Alternative must be one of \"two.sided\",\"greater\" or \"less\"")
    }

    return(list(statistic = Z.star, p.value = P.value, METHOD <- method))}
  }}


