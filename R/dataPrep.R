#' Data preparation Process
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
dataPrep <- function(data){

  vecn1 <- data[!is.na(data[1]) & !is.na(data[2])]
  vecn2 <- data[!is.na(data[1]) & is.na(data[2])]
  vecn3 <- data[is.na(data[1]) & !is.na(data[2])]

  # length of n1, n2 and n3
  n1 <- nrow(vecn1)
  n2 <- length(vecn2[!is.na(vecn2)])
  n3 <- length(vecn3[!is.na(vecn3)])

  n1.matrix <- matrix(vecn1, ncol = 2)


}
