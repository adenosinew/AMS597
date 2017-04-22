#' Output well-organized list for test
#'
#' Only Designed for internal use within the package.
#'
#' @param statistic The value of the Z-statistic
#' @param pv The p value for the test
#' @param METHOD a character string indicating what type of Z-test was performed
#'
#' @return
#'
#'
#' @examples
output <- function(statistic=NULL, pv, METHOD){
  structure(list(statistic = statistic, p.value = pv, method = METHOD))
}
