
#' Title
#'
#' @param dataset Data to be used
#' @param direction Direction of prediction across endpoints. Increase (all increase), decrease (all decrease), mixed (provide a vector of predictions for each variable)
#' @param bound Whether or not a bound will be used
#' @param variables Endpoints of interest
#' @param type Type of analysis, pre-post or group
#' @param gtvar Variable denoting either the group or the time, dependent on type
#' @param phi_0 The null hypothesized value
#' @param predictions A matrix with two columns. First column provides the variable names, second column the directional prediction.
#' @param location Measure of central tendency, mean or median
#'
#' @return A list of two elements. The first element is an indicator for whether each prediction on a variable was correct, the second element is the observed difference between groups or pre-post.
#' @export
#' @import dplyr
#' @importFrom robustbase colMedians
#' @examples
#'
#'
predresults <- function(dataset, direction, bound = "wilcoxon", variables, type = "group",
                               gtvar,  phi_0 = 0.50, predictions, location = "mean"){



  if (!is.numeric(dataset[,gtvar]) )
  {
    stop("gtvar must be numeric, either a numeric variable for time or group (e.g., group 1 vs group 0).")

  }

  if (type == "group"){
    levels <- unique(factor(dataset[,gtvar]))

    if (location == "mean")
    {
      dataset %>%
        group_by(!!as.name(gtvar)) %>%
        summarise_at(all_of(variables), mean, na.rm = TRUE) -> groupmeans
    } else if (location == "median")
    {
      dataset %>%
        group_by(!!as.name(gtvar)) %>%
        summarise_at(all_of(variables), median, na.rm = TRUE) -> groupmeans
    }
    groupmeans <- groupmeans[order(groupmeans$group),] # Prediction calculate as lower group value - higher

    results <- groupmeans[1,variables] - groupmeans[2,variables]
    differences <- results

    if (direction == "increase"){
      for ( z in 1:length(variables)){
        results[c(variables[z])] <- ifelse(results[c(variables[z])] > 0,1,0 )
        predictions <- rep(phi_0, length(variables))
      }
    }else if (direction == "decrease"){
      for ( z in 1:length(variables)){
        results[c(variables[z])] <- ifelse(results[c(variables[z])] < 0,1,0 )
        predictions <- rep(phi_0, length(variables))
      }
    }else if (direction == "mixed"){

      for ( j in 1:dim(predictions)[1])
      {

        rules <- predictions[predictions[,1] == names(results[j]),]
        if (rules[2] == "increase"){

          results[j] <- ifelse(results[j] > 0, 1, 0)

        }else if (rules[2] == "decrease"){

          results[j] <- ifelse(results[j] < 0, 1, 0)

        }else if(rules[2] == "difference"){

          if (bound == "wilcoxon")
          {
            split <- dataset[,c(gtvar,names(results[j]))]

            split1 <- split[split[,gtvar]==as.numeric(as.character(levels[1])),]
            split2 <- split[split[,gtvar]==as.numeric(as.character(levels[2])),]

            results[j] <- ifelse( wilcox.test(split1[,names(results[j])],split2[,names(results[j])])$p.value <
                                   phi_0, 1,0)


          } else if (bound == "normal")
          {
            split <- dataset[,c(gtvar,names(results[j]))]
            split1 <- split[split[,gtvar]==as.numeric(as.character(levels[1])),]
            split2 <- split[split[,gtvar]==as.numeric(as.character(levels[2])),]

            obdiff <- mean(split1[,names(results[j])]) - mean(split2[,names(results[j])])
            results[j] <- ifelse(obdiff >  qnorm(1-(phi_0/2)) | obdiff < qnorm(phi_0/2), 1, 0)
          }
        }
      }
    }
  }else if (type == "prepost"){
    reference <- min(dataset$time)

    post <- dataset[dataset$time != reference,]
    pre <- dataset[dataset$time == reference,]
    if (location == "mean")
    {
      results <- colMeans(as.data.frame(post[,variables] - pre[,variables]))
    } else if (location == "median")
    {
      results <- colMedians(as.matrix(post[,variables] - pre[,variables]))
    }
    differences <- results

    if (direction == "increase"){
      for ( z in 1:length(variables)){
        results[c(variables[z])] <- ifelse(results[c(variables[z])] > 0,1,0 )
      }
    }else if (direction == "decrease"){
      for ( z in 1:length(variables)){
        results[c(variables[z])] <- ifelse(results[c(variables[z])] < 0,1,0 )
      }
    }else if (direction == "mixed"){

      for ( j in 1:dim(predictions)[1])
      {

        rules <- predictions[predictions[,1] ==names(results[j]),]
        if (rules[2] == "increase"){
          results[j] <- ifelse(results[j] > 0, 1, 0)

        }else if (rules[2] == "decrease"){

          results[j] <- ifelse(results[j] < 0, 1, 0)

        }else if(rules[2] == "difference"){

          if (bound == "wilcoxon")
          {
            split <- dataset[,c(gtvar,names(results[j]))]
            split1 <- split[split[,gtvar]==as.numeric(reference),]
            split2 <- split[split[,gtvar]!=as.numeric(reference),]

            results[j] <- ifelse(wilcox.test(split1[,names(results[j])],split2[,names(results[j])])$p.value < phi_0, 1,0)


          } else if (bound == "normal")
          {
            split <- dataset[,c(gtvar,names(results[j]))]
            split1 <- split[split[,gtvar]==as.numeric(reference),]
            split2 <- split[split[,gtvar]!=as.numeric(reference),]

            obdiff <- mean(split2[,names(results[j])]) - mean(split1[,names(results[j])])
            results[j] <- ifelse(obdiff >  qnorm(1-(phi_0/2)) | obdiff < qnorm(phi_0/2), 1, 0)
          }
        }
      }
    }
  }

  outresults <- list(results, differences, variables)
  return(outresults)

}
