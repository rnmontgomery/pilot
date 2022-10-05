
#' Title
#'
#' @param weights A vector of weights for each endpoint
#' @param results A vector of 1's and 0's for each endpoint (the prediction results)
#' @param nullphi The null hypothesized phi value
#' @param alpha The type I error rate
#' @param sims The number of bootstrap simulations
#'
#' @return
#' @export
#'
#' @examples
#'
#' weights <- c(0.25, 0.3, 0.6, 0.15, 0.76, 0.17, 0.23)
#' results <- c(1, 1, 1, 0, 0, 1, 1)
#' nullphi <- 0.50
#' alpha <- 0.05
#' sims <- 100
predboot<- function(weights, results, nullphi = 0.50, alpha = 0.05, sims = 5000){

  teststat <- weights%*%results
  ntests <- length(weights)
  correct <- sum(results)

  if (length(nullphi) == 1 | length(nullphi) == ntests)
  {
    boots <- matrix(NA,sims,1)
    for (g in 1:sims)
    {
      boots[g,] <- ifelse((rbinom(ntests,1,nullphi))%*%weights >= teststat,1,0)
    }
    pval <- mean(boots)

  } else{
    stop("nullphi needs to be either a single value or specified for every endpoint")
  }

  x <- list(statistic = teststat, p.value = pval, null.value = nullphi, m = ntests, alpha = alpha, correct = correct, sims = sims)

  print.boot <- function(x){
    cat("\n\t\tResults for the Prediction Test")

    cat("\n\nOf the", paste(x$m), " endpoints of interest,", paste(x$correct), "were correctly predicted.")
    cat("\nCalculated using a bootstrap of", paste(x$sims), "simulations")

    cat("\n----------------------------------------------------------------")
    cat("\nTm:", paste(format(x$statistic,digits=4)),
        "\t Null hypothesized", paste("\U03D5:"), paste(format(x$null.value)),
        "\t p.value:", paste(format(x$p.value, digits = 4)))
  }
  return(print.boot(x))


}
