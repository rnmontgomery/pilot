
# Adding function to calculate prediction test adjusted for covariates.
# Testing in another script

#' Title
#'
#' @param data Data set
#' @param variables Variables of interest
#' @param covariates Covariates
#' @param id Id variable
#' @param timevar Variable denoting time, only used for pre-post data
#' @param type Type of analysis, pre-post, or group comparison
#' @param cor Method to estimate correlation, sigma uses the estimated covariance matrix from multivariate regression for group and for pre-post the model estimated predictions are subtracted from the raw values and the correlation matrix is calculated from those, resid, uses the residuals (only for group comparisons).  .
#' @importFrom stats lm as.formula resid cor cov2cor median
#' @return A list of vectors for results, observed differences, variable names and weights.
#' @export
#'
#' @examples
#'
#'
#'
predadjusted <- function(dataset, variables, covariates, id, type = "group", gtvar,
                         phi_0 = 0.50, direction,predictions,  corM = "sigma", location = "median"){

  nends <- length(variables)
  n <- dim(dataset)[1] # Assuming data set is in long format
  levels <- unique(factor(dataset[,gtvar]))


  if (!is.numeric(dataset[,gtvar]) )
  {
    stop("gtvar must be numeric, either a numeric variable for time or group (e.g., group 1 vs group 0).")

  }


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
      rowC <- 2+length(covariates) # Intercept + gtvar (2) + number of covariates and gtvar is last

      x <- summary(lm(as.formula(paste(variables[i],"~",betas,"+", gtvar, sep = "")), data = dataset))
      results[,i] <- x$coefficients[rowC,1]
      resids[,i] <- as.vector(resid(x))
    }
      colnames(results) <- c(variables)
      weights <- 1/(rowSums(cor(resids)^2))
      differences <- results


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
      }else if (direction == "mixed"){

        for ( j in 1:dim(predictions)[1])
        {

          rules <- predictions[predictions[,1] == colnames(results)[j],]
          if (rules[2] == "increase"){

            results[j] <- ifelse(results[j] > 0, 1, 0)

          }else if (rules[2] == "decrease"){

            results[j] <- ifelse(results[j] < 0, 1, 0)

          }
        }
      }

      } else if (corM == "sigma")  {
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

          results[1,] <- Betahat[dim(Betahat)[1],] # Not sure this is the right row for Betahat??
          colnames(results) <- c(variables)

          differences <- results
          weights <- 1/rowSums(rhohat^2)

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
          }else if (direction == "mixed"){

            for ( j in 1:dim(predictions)[1])
            {

              rules <- predictions[predictions[,1] == colnames(results)[j],]
              if (rules[2] == "increase"){

                results[j] <- ifelse(results[j] > 0, 1, 0)

              }else if (rules[2] == "decrease"){

                results[j] <- ifelse(results[j] < 0, 1, 0)

              }
            }
          }
      }
  } else if (type == "prepost"){

    if(length(levels)>2){
      stop("There are more than two time points")
    }

    if (corM == "resid"){


      stop("The 'resid' option is only available for type = 'group'.")


    }else if (corM == "sigma"){

      results <- matrix(NA,1,nends)

      pre <- dataset[dataset[, gtvar] ==  levels[1],]
      post <- dataset[dataset[, gtvar] ==  levels[2],]

      diff <- post[,variables]-pre[, variables] # This is the observed post-pre difference

      # Building the design matrix
      # Number of columns is 1(Intercept) + number of covariates

      # For pre-post data, we will only be using baseline adjusted values,
      # i.e,. only the values from the pre time point

      Xdesign <- matrix(NA,dim(pre)[1], 1+length(covariates) )
      Xdesign[,1] <- 1
      Xdesign[,2:(1+length(covariates))] <-  as.matrix(pre[,covariates])

      # Response matrix
      Ymat <- as.matrix(diff)

      # Beta hat matrix
      Betahat <- solve(t(Xdesign)%*%Xdesign)%*%t(Xdesign)%*%Ymat


      # Sample covariance matrix:
      #sigmahat <- (t(Ymat)%*%Ymat - t(Betahat)%*%t(Xdesign)%*%Ymat)/(n-length(covariates)-1 )

      # Convert to correlation matrix
      #rhohat <- cov2cor(sigmahat)


      # Given betahat get predicted values for the difference
      predicted <- Xdesign %*% Betahat


      rhohat <- cor(predicted) # This is the correlation matrix for the post-pre difference
                        # when we subtract away the expected value based on
                        # the covariate effects


      # the location is what we use to determine whether the prediction was correct
      # for mean this is equivalent to using the raw data since the mean of the predicted
      # values is equal to the mean of the raw data. For median this is the column
      # medians of the predicted value.

      if (location == "median"){

        results[1,] <- apply(predicted,2,median)

      } else if (location == "mean"){

        results[1,] <- colMeans(predicted)
      }

      colnames(results) <- c(variables)
      weights <- 1/rowSums(rhohat^2)
      differences <- results


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


  }


    results <- as.vector(results)
    weights <- as.vector(weights)
    outlist <- list(results, differences, variables, weights)
    return(outlist)
  }

  #}













