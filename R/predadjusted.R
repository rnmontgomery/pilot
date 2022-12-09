
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
#' @param cor Method to estimate correlation, sigma uses the estimated covariance matrix from multivariate regression, resid, uses the residuals.
#'
#' @return
#' @export
#'
#' @examples
predadjusted <- function(data, variables, covariates, id, type = "group", gtvar,
                         phi_0 = 0.50, direction, predictions,  corM = "sigma"){

  nends <- length(variables)
  n <- dim(dataset)[1] # Assuming data set is in long format
  levels <- unique(factor(dataset[,gtvar]))


  if (type == "group"){

    if(length(levels)>2){
      stop("There are more than two groups.")
    }


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

      } else if (corM == "sigma")
      {
        results <- matrix(NA,1,nends)

          # Building the design matrix
          # Number of columns is 1(Intercept) + number of covariates + variables for group/pre-post
          Xdesign <- matrix(NA,n, 1+length(covariates)+1 )
          Xdesign[,1] <- 1
          Xdesign[,2:(1+length(covariates))] <-  as.matrix(dataset[,covariates])
          Xdesign[,1+length(covariates)+1] <- as.matrix(dataset[,gtvar])

          # Response matrix
          Ymat <- as.matrix(dataset[,variables])

          # Beta hat matrix
          Betahat <- solve(t(Xdesign)%*%Xdesign)%*%t(Xdesign)%*%Ymat

          # Sample covariance matrix:
          sigmahat <- (t(Ymat)%*%Ymat - t(Betahat)%*%t(Xdesign)%*%Ymat)/(n-length(covariates)-1 )

          # Convert to correlation matrix
          rhohat <- cov2cor(sigmahat)

          results[1,] <- Betahat[dim(Betahat)[1],]
          colnames(results) <- c(variables)
          weights <- 1/rowSums(cor(Ymat)^2)

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
        }
      }
  } else if (type == "prepost"){

    if(length(levels)>2){
      stop("There are more than two time points")
    }

    if (corM == "resid"){





    }else if (corM == "sigma")
    {

      results <- matrix(NA,1,nends)

      pre <- dataset[dataset[, gtvar] ==  levels[1],]
      post <- dataset[dataset[, gtvar] ==  levels[2],]

      diff <- post[,variables]-pre[, variables]

      # Building the design matrix
      # Number of columns is 1(Intercept) + number of covariates
      Xdesign <- matrix(NA,n, 1+length(covariates) )
      Xdesign[,1] <- 1
      Xdesign[,2:(1+length(covariates))] <-  as.matrix(dataset[,covariates])


      # Design matrix and response matrix are no the right dimension
      Ymat <- as.matrix(diff)

      # Beta hat matrix
      Betahat <- solve(t(Xdesign)%*%Xdesign)%*%t(Xdesign)%*%Ymat

      # Sample covariance matrix:
      sigmahat <- (t(Ymat)%*%Ymat - t(Betahat)%*%t(Xdesign)%*%Ymat)/(n-length(covariates)-1 )

      # Convert to correlation matrix
      rhohat <- cov2cor(sigmahat)


    }


  }




    results <- as.vector(results)
    weights <- as.vector(weights)
    outlist <- list(results,weights, variables)
    return(outlist)
  }

}













