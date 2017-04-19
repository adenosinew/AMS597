#' modified t-statistic of Kim et al.
#'
#' @param data
#' @param alternative
#'
#' @return
#' @export
#'
#' @examples
modTstat <-
  function(data,
           alternative = c("two.sided", "less", "greater")) {
    METHOD <- "Modified t-statistic of Kim et al."

    # Prepare the data
    source(dataPrep(data))

    # calculate mean difference of n1 and mean of n2 and n3
    # n1 group
    Dbar <- mean(n1.matrix[, 1] - n1.matrix[, 2])
    # n2 group
    Tbar <- mean(vecn2, na.rm = TRUE)
    # n3 group
    Nbar <- mean(vecn3, na.rm = TRUE)

    # calculate sample standard deviations
    # n1 group
    SD <- sd(n1.matrix[, 1] - n1.matrix[, 2])
    # n2 group
    ST <- sd(vecn2, na.rm = TRUE)
    # n3 group
    SN <- sd(vecn3, na.rm = TRUE)

    #* calculate harmonic mean
    nh <- 2 / (1 / n2 + 1 / n3)

    # calculate t3
    t3 <-
      (n1 * Dbar + nh * (Tbar - Nbar)) / sqrt(n1 * (SD ^ 2) + (nh ^ 2) * ((SN ^
                                                                               2) / n3 + (ST ^ 2) / n2))

    # calculate pvalue
    if (alternative == "two.sided") {
      pv <- 2 * pnorm(abs(t3), lower.tail = FALSE)
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
    return(list(
      statistic <-  t3,
      p.value <- pv,
      method = METHOD
    ))

  }
