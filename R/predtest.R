

#' Title
#'
#' @param weights A vector of weights for each endpoint
#' @param results A vector of 1's and 0's (the results for each endpoint)
#' @param nullphi Null hypothesized phi value
#' @param alpha Type I error rate
#' @param exact Should the calculation be exact? Supported for m < 20
#'
#' @return A printed list
#' @export
#'
#' @examples
#' weights <- c(0.25, 0.3, 0.6, 0.15, 0.76, 0.17, 0.23)
#' results <- c(1, 1, 1, 0, 0, 1, 1)
#' nullphi <- 0.50
#' alpha <- 0.05
#' exact <- TRUE
predtest<- function(weights, results, nullphi = 0.50, alpha = 0.05, exact = TRUE){

  ntests <- length(weights)
  results <- unlist(results)
  teststat <- as.numeric(weights%*%results)
  correct <- sum(results)

  if (exact == FALSE){
    type = 1
    z <- (teststat - nullphi*(sum(weights)) )/sqrt(nullphi*(1-nullphi)*sum(weights^2))
    pval <- 1-stats::pnorm(z)
    if (pval < alpha)
    {
      decision <- 1
    } else if (pval >= alpha){
      decision <- 0
    }
  } else if( exact == TRUE & ntests < 25)
  {
    type = 2
    nperm <- 2^ntests
    perms <- as.matrix(expand.grid(rep(list(0:1), ntests)))
    values <- perms%*%as.matrix(weights)
    rank <- as.data.frame(cbind(values,rank(values)))
    pval <- 1-(rank[which(rank$V1 == as.numeric(teststat)),2]/nperm)
    if (pval < alpha)
    {
      decision <- 1
    } else if (pval >= alpha){
      decision <- 0
    }

  } else if (exact == TRUE & ntests >= 25)
  {
    type = 2
    stop("The exact test is only available for 25 or fewer endpoints due to the large number of permutations of the test statistic.")
  }

  x <- list(statistic = teststat, p.value = pval, null.value = nullphi, m = ntests, alpha = alpha, correct = correct, type = type )


  print.prediction.test <- function(x){
    cat("\n\t\tResults for the Prediction Test")

    cat("\n\nOf the", paste(x$m), " endpoints of interest,", paste(x$correct), "were correctly predicted.")
    if (x$type == 1){
      cat("\nCalculated using the normal approximation:")
    } else if (x$type == 2){
      cat("\nCalculated using the exact distribution:")
    }
    cat("\n----------------------------------------------------------------")
    cat("\nTm:", paste(format(x$statistic,digits=4)),
        "\t Null hypothesized", paste("\U03D5:"), paste(format(x$null.value)),
        "\t p.value:", paste(format(x$p.value, digits = 4)))
  }


  return(print.prediction.test(x))

}
