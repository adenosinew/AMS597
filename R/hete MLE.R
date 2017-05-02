#' MLE based test of Lin and Stivers under heteroscedasticity
#'
#' @param x a (non-empty) numeric vector of data values. For tumor data.
#' @param y a(non-empty) numeric vector of data values. For normal data
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or"less". You can specify just the initial letter.
#'
#' @return A list containing the following components
#' @return \code{statistic} the value of the corrected Z-test statistic.
#' @return \code{p.value} the p-value for the test.
#' @export
#'
#' @examples
#' heteMLE(x,y,alternative="greater")

heteMLE <- function(x, y, alternative = "two.sided") {
  if (length(x) != length(y)) {
    warning("tumor sample and normal sample have different dimension. Use two sample t test")
    Ttest <- t.test(x, y, alternative = alternative)
    return(
      list(
        statistic =
          Ttest$statistic,
        parameter = Ttest$parameter,
        p.value = Ttest$p.value,
        METHOD = "Two sample T test"
      )
    )
  }
  else{
    paired.x <- x[!is.na(x) == !is.na(y)]
    paired.y <- y[!is.na(x) == !is.na(y)]
    tumor <- x[!is.na(x) & is.na(y)]
    normal <- y[is.na(x) & !is.na(y)]
    n1 <- length(paired.x)
    n2 <- length(tumor)
    n3 <- length(normal)

    if (n1 == 0) {
      warning("No paired sample found. Use two sample t test")
      Ttest <- t.test(x, y, alternative = alternative)
      return(
        list(
          statistic =
            Ttest$statistic,
          parameter = Ttest$parameter,
          p.value = Ttest$p.value,
          METHOD = "Two sample T test"
        )
      )
    }

    else if (n2 == 0 & n3 == 0) {
      warning("No unpaired sample found. Use paired t test")
      Ttest <- t.test(x, y, alternative = alternative,paired = TRUE)
      return(
        list(
          statistic =
            Ttest$statistic,
          parameter = Ttest$parameter,
          p.value = Ttest$p.value,
          METHOD = "Two sample T test"
        )
      )
    }
    else if (n1<=1|n2<=1|n3<=1){
      warning("Some observation is too small, Use two sample t test")
      Ttest <- t.test(x, y, alternative = alternative)
      return(
        list(
          statistic =
            Ttest$statistic,
          parameter = Ttest$parameter,
          p.value = Ttest$p.value,
          METHOD = "Two sample T test"
        )
      )
    }
    else{
      method <-
        "MLE based test of Lin and Stivers under heteroscedasticity"
      T_bar <- mean(tumor)
      N_bar <- mean(normal)
      sd_T <- sd(tumor)
      sd_N <- sd(normal)
      T1_bar <- mean(paired.x)
      N1_bar <- mean(paired.y)
      sd_T1 <- sd(paired.x)
      sd_N1 <- sd(paired.y)
      sd_TN1 <- cov(paired.x, paired.y)

      r = sd_TN1 / (sd_N1 * sd_T1)
      f = n1 * (n1 + n3 + n2 * sd_TN1 / (sd_T1 ^ 2)) * ((n1 + n2) * (n1 +
                                                                       n3) - n2 * n3 * r ^ 2) ^ (-1)
      g = n1 * (n1 + n2 + n3 * sd_TN1 / (sd_N1 ^ 2)) * ((n1 + n2) * (n1 +
                                                                       n3) - n2 * n3 * r ^ 2) ^ (-1)
      v1_1 = ((f ^ 2) / n1 + ((1 - f) ^ 2) / n2) * (sd_T1 ^ 2) * (n1 - 1)
      v1_2 = ((g ^ 2) / n1 + ((1 - g) ^ 2) / n3) * (sd_N1 ^ 2) * (n1 - 1)
      v1_3 = 2 * f * g * sd_TN1 * (n1 - 1) / n1
      v1 = (v1_1 + v1_2 - v1_3) / (n1 - 1)
      denominator = sqrt(v1)

      Z.ls = (f * (T1_bar - T_bar) - g * (N1_bar - N_bar) + T_bar - N_bar) /
        denominator

      if (alternative == "greater") {
        P.value = pt(Z.ls, df = n1, lower.tail = F)
      }
      else if (alternative == "less") {
        P.value = pt(Z.ls, df = n1, lower.tail = T)
      }
      else if (alternative == "two.sided") {
        P.value = 2 * pt(abs(Z.ls), df = n1, lower.tail = F)
      }
      # else{
      #   warning(
      #     "The input for parameter \"alternative\" is not correct. It can only be \"greater\", \"less\" or \"two.sided\"."
      #   )
      # }
      return(list(
        statistic = Z.ls,
        parameter = n1,
        p.value = P.value,
        METHOD = method
      ))
    }

  }
}
