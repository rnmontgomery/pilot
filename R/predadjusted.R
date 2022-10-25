
# Adding function to calculate prediction test adjusted for covariates.
# Testing in another script

#' Title
#'
#' @param data Data set
#' @param variables Variables of interest
#' @param covariates Covariates to adjust for
#' @param id Id variable
#' @param timevar Variable denoting time, only used for pre-post data
#' @param type Type of analysis, pre-post, or group comparison
#' @param cor Type of correlation, default is Pearson
#'
#' @return
#' @export
#'
#' @examples
predadjusted <- function(data, variables, covariates, id, type = "group", gtvar,
                         phi_0 = 0.50, direction, predictions,  corM = "resid"){

  nends <- length(variables)


  # Need to get results
  # Need to get weights (this is different)
  # Need to then pass the weights and results to the test function

  if (type == "group"){
    levels <- unique(factor(dataset[,gtvar]))

      if (corM == "resid"){

    results <- matrix(NA,1,nends)
    resids <- matrix(NA,dim(dataset)[1],nends)
    for (i in 1:nends)
    {
      v <- as.vector(covariates, mode = "any")
      betas <- paste(v, collapse = "+")
      rowC <- 2+length(covariates)

      x <- summary(lm(as.formula(paste(variables[i],"~",betas,"+", gtvar, sep = "")), data = dataset))
      results[,i] <- x$coefficients[rowC,1]
      resids[,i] <- as.vector(resid(x))
    }
      colnames(results) <- c(variables)
      weights <- 1/(rowSums(cor(resids)^2))

      if (direction == "increase"){
        for ( z in 1:length(variables)){
          results[,c(variables[z])] <- ifelse(results[,c(variables[z])] > 0,1,0 )
          predictions <- rep(phi_0, length(variables))
        }
      }else if (direction == "decrease"){
        for ( z in 1:length(variables)){
          results[,c(variables[z])] <- ifelse(results[,c(variables[z])] < 0,1,0 )
          predictions <- rep(phi_0, length(variables))
        }

    # So far, I've gotten the correct coefficient
    # Next compare to increase/decrease, maybe skip mixed for now (how would difference be defined?)
    # Then get weights, from the resids, so I need to store those actually,
    # Then output the adjusted weights and results which can be passed to the predtest or predboot function.

      } else if (corM == "other")
      {


      }
  } else if (type == "prepost"){ # type = group



  }





    outlist <- list(results,weights)
    return(outlist)
  }

}













