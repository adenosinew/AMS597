#' P value for Z-test
#' Only Designed for internal use within the package.
#' @param statistic The value of the Z-statistic
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#'
#' @return p value
#'
#'
#' @examples
pval <- function(statistic, alternative=c("two.sided", "less", "greater")){

  if (alternative == "two.sided") {
    pv <- 2 * pnorm(abs(statistic), lower.tail = FALSE)
  }
  if (alternative == "less") {
    pv <- pnorm(t3, lower.tail = TRUE)
  }
  if (alternative == "greater") {
    pv <- pnorm(t3, lower.tail = FALSE)
  }
  else{
    warning("alternative must be two.sided, less or greater")
  }

}
