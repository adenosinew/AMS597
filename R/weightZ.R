#' Weighted Z-test Combination
#'
#' @param data
#' @param alternative
#'
#' @return
#' @export
#'
#' @examples
weightZ <- function(data, alternative = c("two.sided", "less", "greater")){
  # Prepare the data
  source(dataPrep(data))

  # Calculate weight Liptak[12] Square root of the sample sizes
  w1 <- sqrt(n1)
  w2 <- sqrt(n2 + n3)

  # Calculate Paired sample and two-sample t-tests
  #* should I use t.test() with paired argument or pairwise.t.test()?
  pair.sample.test <- t.test(n1.matrix[,1], n1.matrix[,2], alternative, paired = TRUE)
  p1 <- pair.sample.test$p.value
  #* should I check the var.equal in this case?
  two.sample.test <- t.test(n1.matrix[,1], n1.matrix[,2], alternative, paired = FALSE)
  p2 <- two.sample.test$p.value

  # Calculate Za from p value
  z1 <- qnorm(1-p1)
  z2 <- qnorm(1-p2)

  # Calculate combined p value
  pc <- 1 - pnorm((w1*z1+w2*z2)/sqrt((w1^2)+w2^2))

  # For two sided case
  if (alternative=="two.sided"){
    if (pc<0.5) pc <- 2*pc
    else pc <- 2*(1-pc)
  }

  # Return values
  return(p.value <- pc)
}
