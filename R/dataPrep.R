#' x preparation Process
#'
#' This function read x frame and return common information for further calculation.
#' Only Designed for internal use within the package.
#'
#' @param x data Frame
#'
#' @return Common information for further Call
#'
#'
#' @examples
#' dataPrep(x)
dataPrep <- function(x, alternative){
  if (length(x[,1])!=length(x[,2])){
    print(t.test(x[,1],x[,2],alternative = alternative))
    flag <<- 0
  }
  else{
  vecn1 <<- x[!is.na(x[,1]) & !is.na(x[,2])]
  vecn2 <<- na.omit(x[!is.na(x[,1]) & is.na(x[,2])])
  vecn3 <<- na.omit(x[is.na(x[,1]) & !is.na(x[,2])])

  # length of n1, n2 and n3
  n1.matrix <<- matrix(vecn1, ncol = 2)

  n1 <<- nrow(n1.matrix)
  n2 <<- length(vecn2[!is.na(vecn2)])
  n3 <<- length(vecn3[!is.na(vecn3)])
  if (n1==0){
    print(t.test(n1.matrix[,1],n1.matrix[,2]))
  }
  if (n2==0 | n3==0){
    print(t.test(n1.matrix[,1],n1.matrix[,2],paired = T))
  }

}
}
