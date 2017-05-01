#' Modified t-test
#'
#' @param x value of the "tumor"
#' @param y value of the "normal"
#' @param alternative a character string indicates the alternative hypothesis and must be one of "two.sided", "greater" or "less".
#'
#' @return A list containing the following components:
#' @return \code{statistic} the value of weighted z test combination statistic
#' @return \code{p.value} the p-value for the test
#' @export
#'
#' @examples
#' x <- c(1,2,3,4,NA,NA,1)
#' y <- c(1,2,5)
#' my.mod.t.test(x,y)
modttest = function (x, y, alternative = "two.sided") {
  if (length(x) != length(y)) {
    warning("tumor sample and normal sample have different dimension. Reduce to two sample t test")
    Ttest <- t.test(x, y, alternative = alternative)
    return(
      list(
        statistic <-
          Ttest$statistic,
        p.value <- Ttest$p.value,
        METHOD <- "Two sample T test"
      )
    )
  }
  else {
    paired.x <- x[!is.na(x) == !is.na(y)]
    paired.y <- y[!is.na(y) == !is.na(x)]
    D <- paired.x - paired.y
    tumor <- x[!is.na(x) & is.na(y)]
    normal <- y[is.na(x) & !is.na(y)]
    Dbar <- mean(D[!is.na(D)])
    tumorbar <- mean(tumor[!is.na(tumor)])
    normalbar <- mean(normal[!is.na(normal)])

    n1 = length(paired.x)
    n2 = length(tumor)
    n3 = length(normal)
    nh = 2 * n2 * n3 / (n2 + n3)

    if (n1 == 1 | n2 == 1 | n3 == 1) {
      stop("not enough observations")
    }
    if (n1 == 0) {
      warning("No paired sample found. Reduce to two sample t test")
      Ttest <- t.test(x, y, alternative = alternative)
      return(
        list(
          statistic <-
            Ttest$statistic,
          p.value <- Ttest$p.value,
          METHOD <- "Two sample T test"
        )
      )
    }

    if (n2 == 0 & n3 == 0) {
      warning("No unpaired sample found. Reduce to paired t test")
      Ttest <- t.test(x, y, alternative = alternative, paired = TRUE)
      return(
        list(
          statistic <-
            Ttest$statistic,
          p.value <- Ttest$p.value,
          METHOD <- "Two sample T test"
        )
      )
    }
    else {
      method <- "Modified T test"
      VD <- var(D[!is.na(D)])
      VT <- var(tumor[!is.na(tumor)])
      VN <- var(normal[!is.na(normal)])
      t3 <-
        (n1 * Dbar + nh * (tumorbar - normalbar)) / sqrt(n1 * VD + nh ^ 2 * (VN /
                                                                               n3 + VT / n2))
      if (alternative == "two.sided") {
        p.value = 2 * pnorm(abs(t3), lower.tail = F)
      }
      if (alternative == "greater") {
        p.value = pnorm(t3, lower.tail = F)
      }
      if (alternative == "less") {
        p.value = pnorm(t3, lower.tail = T)
      }
      # else{
      #   warning("Alternative must be one of \"two.sided\",\"greater\" or \"less\"")
      # }
      return(list(statistic = t3, p.value = p.value, METHOD <- method))
    }
  }
}
