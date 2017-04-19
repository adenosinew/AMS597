#' Corrected Z-test of Looney and Jones
#'
#' @param data
#' @param alternative
#'
#' @return
#' @export
#'
#' @examples
corrZtest <- function(data, alternative = c("two.sided", "less", "greater")){

  # Prepare the data
  source(dataPrep(data))

  # Calculate mean tumor and normal
  Tbstar <- mean(c(n1.matrix[,1], vecn2), na.rm = TRUE)
  Nbstar <- mean(c(n1.matrix[,2], vecn3), na.rm = TRUE)

  # Calculate standard deviation and covariance
  STstar <- sd(c(n1.matrix[,1], vecn2), na.rm = TRUE)
  SNstar <- sd(c(n1.matrix[,2], vecn3), na.rm = TRUE)
  STN1 <- cov(n1.matrix)

  # Calculate Corrected Z-test
  Zcorr <-
    (Tbstar - Nbstar) / (sqrt((STstar ^ 2) / (n1 + n2) + (SNstar ^ 2) / (n1 +
                                                                            n3) - 2 * n1 * STN1 / ((n1 + n2) * (n1 + n3))))

  # Calculate P value
  if (alternative == "two.sided") {
    p.value = 2 * pnorm(abs(Zcorr), lower.tail = F)
  }
  if (alternative == "greater") {
    p.value = pnorm(Zcorr, lower.tail = F)
  }
  if (alternative == "less") {
    p.value = pnorm(Zcorr, lower.tail = T)
  }

  return(c(z.stat = Zcorr, p.value = p.value))

}
