#' Title
#'
#' @param data Data set
#' @param variables Variables in the data set you want to include (only endpoints)
#' @param id The ID variable from the data set
#' @param timevar The time variable from the data set
#' @param type The type of analysis (pre-post or between groups)
#' @param cor Type of correlation, e.g. pearson
#'
#' @return A list of weights
#' @export
#'
#' @examples
prediction.weights <- function(data, variables,id, timevar, type = "group", cor = "pearson"){


  if(type == "group")
  {
    dataset <- data
    endpoints <- subset(dataset, select = c(variables))
    samplec <- cor(endpoints, method = cor)
    weights <- rowSums(samplec^2)

  }else if( type == "prepost"){

    dataset <- data
    timevar <- dataset$time

    if (is.null(timevar) )
    {
      stop("Provide a numeric timevar to indicate pre and post observations.")

    }
    if( length(unique(timevar)) > 2 ){

      stop("Expecting two values for timevar.")
    }



    mutatefunc <- function (column) (
      dataset %>%
        arrange(id) %>%
        group_by(id) %>%
        mutate( !!paste0('diff.',as.name(column)) :=  !!as.name(column)-first(!!as.name(column))  ) -> dataset

    )

    for ( i in 1:length(variables))
    {
      x <- variables[i]
      dataset <- mutatefunc(x)
    }

    samplec <- cor( dataset[,c(paste0( "diff.",variables)) ], method = cor)
    weights <- rowSums(samplec^2)



  }
  outweight <- list(weights)
  return(outweight)
}

