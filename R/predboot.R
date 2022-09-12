

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

  x <- list(statistic = teststat, p.value = pval, null.value = nullphi, m = ntests, alpha = alpha, correct = correct )

  return(x)


}
