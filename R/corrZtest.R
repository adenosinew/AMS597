#' Corrected Z-test of Looney and Jones
#'
#' @param data Data Frame
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#'
#' @return
#' @export
#' @keywords stats
#' @examples
#' corrZtest(x)
#' corrZtest(x, "less")
corrZtest <- function(data, alternative = c("two.sided", "less", "greater")){

  METHOD <- "Corrected Z-test of Looney and Jones"

  # Prepare the data
  source(dataPrep(data))

  # Calculate mean tumor and normal
  Tbstar <- mean(c(n1.matrix[,1], vecn2))
  Nbstar <- mean(c(n1.matrix[,2], vecn3))

  # Calculate standard deviation and covariance
  STstar <- sd(c(n1.matrix[,1], vecn2))
  SNstar <- sd(c(n1.matrix[,2], vecn3))
  STN1 <- cov(n1.matrix)

  # Calculate Corrected Z-test
  Zcorr <-
    (Tbstar - Nbstar) / (sqrt((STstar ^ 2) / (n1 + n2) + (SNstar ^ 2) / (n1 +
                                                                            n3) - 2 * n1 * STN1 / ((n1 + n2) * (n1 + n3))))

  # Calculate P value
  pv <- pval(Zcorr, alternative)

  # Output
  output(Zcorr, pv, METHOD)

}
