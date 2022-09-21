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
#' ID <- 1:30
#' buildtesting <- as.data.frame(ID)
#' buildtesting$group <- c(rep(0,15), rep(1,15))
#' buildtesting$v1 <- rnorm(30, 0, 1)
#' buildtesting$v2 <- rnorm(30, 0, 1)
#' buildtesting$v3 <- rnorm(30, 0, 1)
#' buildtesting$v4 <- rnorm(30, 0, 1)
#'
#' ID2 <- c(rep(1:15,2))
#' buildtesting2 <- as.data.frame(ID2)
#' buildtesting2$time2 <- c(rep(0,15), rep(12,15))
#' buildtesting2$v1 <- rnorm(30, 0, 1)
#' buildtesting2$v2 <- rnorm(30, 0, 1)
#' buildtesting2$v3 <- rnorm(30, 0, 1)
#' buildtesting2$v4 <- rnorm(30, 0, 1)
#'
#'
#'
#'
#'
#'


#'
predweights <- function(data, variables, id, timevar, type = "group", cor = "pearson"){


  if(type == "group")
  {
    dataset <- data
    endpoints <- subset(dataset, select = c(variables))
    samplec <- cor(endpoints, method = cor)
    weights <- rowSums(samplec^2)

  }else if( type == "prepost"){

    dataset <- data
    timevard <- dataset[,timevar]


    if (!is.numeric(timevard) )
    {
      stop("Provide a numeric timevar to indicate pre and post observations.")

    }
    if( length(unique(timevard)) > 2 ){

      stop("Only pre-post data is supported with two unique time values.")
    }

    mutatefunc <- function (data,id,column) (
      data %>%
        dplyr::arrange(!!id, timevar) %>%
        dplyr::group_by(id, timevar ) %>%
        dplyr::mutate( !!paste0('diff.',as.name(column)) :=
                         !!as.name(column)-first(!!as.name(column))  ) -> dataset

    )

    for ( i in 1:length(variables))
    {
      x <- variables[i]
      dataset <- mutatefunc(data = dataset, id = id, column = x)
    }

    samplec <- cor( dataset[,c(paste0( "diff.",variables)) ], method = cor)
    weights <- rowSums(samplec^2)

  }
  outweight <- list(weights)
  return(outweight)
}

