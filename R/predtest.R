

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

  return(x)

}
