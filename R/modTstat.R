#' modified t-statistic of Kim et al.
#'
#' @param data Data Frame
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#'
#' @return A list with statistics and p value \code{statis}
#' @export
#'
#' @examples
#' modTstat(x, "less")
modTstat <- function(x,alternative = c("two.sided", "less", "greater")) {

    METHOD <- "Modified t-statistic of Kim et al."

    # Prepare the data
    source(dataPrep(x,alternative))
    # calculate mean difference of n1 and mean of n2 and n3
    # n1 group
    Dbar <- mean(n1.matrix[, 1] - n1.matrix[, 2])
    # n2 group
    Tbar <- mean(vecn2)
    # n3 group
    Nbar <- mean(vecn3)

    # calculate sample standard deviations
    # n1 group
    SD <- sd(n1.matrix[, 1] - n1.matrix[, 2])
    # n2 group
    ST <- sd(vecn2)
    # n3 group
    SN <- sd(vecn3)

    #* calculate harmonic mean
    nh <- 2 / (1 / n2 + 1 / n3)

    # calculate t3
    t3 <-(n1 * Dbar + nh * (Tbar - Nbar)) / sqrt(n1 * (SD ^ 2) + (nh ^ 2) * ((SN ^2) / n3 + (ST ^ 2) / n2))

    # calculate pvalue
    pv <- pval(t3, alternative)

    #output the result
    output(t3, pv, METHOD)

  }
