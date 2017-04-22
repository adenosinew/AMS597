#' Data preparation Process
#'
#' This function read data frame and return common information for further calculation.
#' Only Designed for internal use within the package.
#'
#' @param data Data Frame
#'
#' @return Common information for further Call
#'
#'
#' @examples
#' dataPrep(x)
dataPrep <- function(data){

  vecn1 <- data[!is.na(data[1]) & !is.na(data[2])]
  vecn2 <- na.omit(data[!is.na(data[1]) & is.na(data[2])])
  vecn3 <- na.omit(data[is.na(data[1]) & !is.na(data[2])])

  # length of n1, n2 and n3
  n1 <- nrow(n1.matrix)
  n2 <- length(vecn2[!is.na(vecn2)])
  n3 <- length(vecn3[!is.na(vecn3)])

  n1.matrix <- matrix(vecn1, ncol = 2)


}
