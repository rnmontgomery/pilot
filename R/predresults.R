
#' Title
#'
#' @param dataset
#' @param direction
#' @param bound
#' @param variables
#' @param type
#' @param gtvar
#' @param phi_0
#' @param predictions
#' @param location
#'
#' @return
#' @export
#' @import dplyr
#' @examples
#'
#'
predresults <- function(dataset, direction, bound = "wilcoxon", variables, type = "group",
                               gtvar,  phi_0 = 0.50, predictions, location = "mean"){

  if (type == "group"){
    levels <- unique(factor(dataset[,gtvar]))

    if (location == "mean")
    {
      dataset %>%
        group_by(group) %>%
        summarise_at(all_of(variables), mean, na.rm = TRUE) -> groupmeans
    } else if (location == "median")
    {
      dataset %>%
        group_by(group) %>%
        summarise_at(all_of(variables), median, na.rm = TRUE) -> groupmeans
    }
    groupmeans <- groupmeans[order(groupmeans$group),] # Prediction calculate as Grp1-Grp2

    results <- groupmeans[1,variables] - groupmeans[2,variables]
    differences <- results

    if (direction == "up"){
      for ( z in 1:length(variables)){
        results[c(variables[z])] <- ifelse(results[c(variables[z])] > 0,1,0 )
        predictions <- rep(phi_0, length(variables))
      }
    }else if (direction == "down"){
      for ( z in 1:length(variables)){
        results[c(variables[z])] <- ifelse(results[c(variables[z])] < 0,1,0 )
        predictions <- rep(phi_0, length(variables))
      }
    }else if (direction == "mixed"){

      for ( j in 1:dim(predictions)[1])
      {

        rules <- predictions[predictions[,1] ==names(results[j]),]
        if (as.numeric(rules[2]) == 1){

          results[j] <- ifelse(results[j] > 0, 1, 0)

        }else if (as.numeric(rules[2]) == 2){

          results[j] <- ifelse(results[j] < 0, 1, 0)

        }else if(as.numeric(rules[2]) == 3){

          if (bound == "wilcoxon")
          {
            split <- dataset[,c(gtvar,names(results[j]))]
            split1 <- split[split[,gtvar]==as.numeric(levels[1]),]
            split2 <- split[split[,gtvar]==as.numeric(levels[2]),]

            results[j] <- ifelse(wilcox.test(split1[,names(results[j])],split2[,names(results[j])])$p.value <
                                   phi_0, 1,0)


          } else if (bound == "normal")
          {
            split <- dataset[,c(gtvar,names(results[j]))]
            split1 <- split[split[,gtvar]==as.numeric(levels[1]),]
            split2 <- split[split[,gtvar]==as.numeric(levels[2]),]

            obdiff <- mean(split1[,names(results[j])]) - mean(split2[,names(results[j])])
            results[j] <- ifelse(obdiff >  qnorm(1-(phi_0/2)) | obdiff < qnorm(phi_0/2), 1, 0)
          }
        }
      }
    }
  }else if (type == "prepost"){
    reference <- factor(dataset$time)[1]

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

    if (direction == "up"){
      for ( z in 1:length(variables)){
        results[c(variables[z])] <- ifelse(results[c(variables[z])] > 0,1,0 )
      }
    }else if (direction == "down"){
      for ( z in 1:length(variables)){
        results[c(variables[z])] <- ifelse(results[c(variables[z])] < 0,1,0 )
      }
    }else if (direction == "mixed"){

      for ( j in 1:dim(predictions)[1])
      {

        rules <- predictions[predictions[,1] ==names(results[j]),]
        if (as.numeric(rules[2]) == 1){
          results[j] <- ifelse(results[j] > 0, 1, 0)

        }else if (as.numeric(rules[2]) == 2){

          results[j] <- ifelse(results[j] < 0, 1, 0)

        }else if(as.numeric(rules[2]) == 3){

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

  outresults <- list(results, differences)
  return(outresults)

}
