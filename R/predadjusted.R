
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
                         phi_0 = 0.50, predictions,  corM = resid){

  nends <- length(variables)


  # Need to get results
  # Need to get weights (this is different)
  # Need to then pass the weights and results to the test function

  if (type == "group"){
    levels <- unique(factor(dataset[,gtvar]))

    for (i in 1:length(nends))
    {
      v <- as.vector(covariates, mode = "any")
      betas <- paste(v, collapse = "+")
      rowC <- 2+length(covariates)
      summary(lm(as.formula(paste(variables[i],"~",betas,"+", gtvar, sep = "")), data = dataset))$coefficients[rowC,1]

    }

    # So far, I've gotten the correct coefficient
    # Next compare to increase/decrease, maybe skip mixed for now (how would difference be defined?)
    # Then get weights, from the resids, so I need to store those actually,
    # Then output the adjusted weights and results which can be passed to the predtest or predboot function.


  } else if (type = "prepost"){ # type = group



  }







}













