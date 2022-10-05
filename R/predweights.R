#' Title
#'
#' @param data Data set
#' @param variables Variables in the data set you want to include (only endpoints)
#' @param id The ID variable from the data set
#' @param timevar The time variable from the data set
#' @param type The type of analysis (pre-post or between groups)
#' @param cor Type of correlation, default is pearson
#'
#' @return A list of weights, named the original variable names (type: group), or diff.variable name (type: prepost)
#' @importFrom rlang :=
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

predweights <- function(data, variables, id, type = "group",timevar, cor = "pearson"){




  if(type == "group")
  {
    endpoints <- data[, c(variables)]
    samplec <- cor(endpoints, method = cor)
    weights <- 1/rowSums(samplec^2)

  }else if( type == "prepost"){

    timevard <- data[,timevar]


    if (!is.numeric(timevard) )
    {
      stop("Provide a numeric timevar to indicate pre and post observations.")

    }
    if( length(unique(timevard)) > 2 ){

      stop("Only pre-post data is supported with two unique time values.")
    }

    #  A little hacky but hopefully it works
    varlist <- paste0("diff.", variables)

    mutatefunc <- function (data,id,column, newname) (
      data %>%
        dplyr::group_by(!!as.name(id) ) %>%
        dplyr::mutate(!!as.name(newname) := !!as.name(column) - first(!!as.name(column)))

    )


    for ( i in 1:length(variables))
    {
      vars <- variables[i]
      flist <- varlist[i]
      data <- mutatefunc(data, id,  column = vars, newname = flist)
    }
    datadiffs <- data[data[,(timevar)] == unique(timevard)[2],]


    samplec <- cor( datadiffs[,c(varlist) ], method = cor)
    weights <- 1/rowSums(samplec^2)

  }
  outweight <- list(weights)
  return(outweight)
}

