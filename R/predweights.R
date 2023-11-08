#' Title
#'
#' @param dataset Data set
#' @param variables Variables in the data set you want to include (only endpoints)
#' @param id The ID variable from the data set
#' @param gtvar The group or time variable from the data set
#' @param type The type of analysis (pre-post or between groups)
#' @param cor Type of correlation, default is pearson
#'
#' @return A list of weights, named the original variable names (type: group), or diff.variable name (type: prepost)
#' @importFrom rlang :=
#' @import dplyr
#' @export
#' @importFrom lme4 lmer
#' @examples
#' ID <- 1:30
#' buildtesting <- as.data.frame(ID)
#' buildtesting$group <- c(rep(0,15), rep(1,15))
#' buildtesting$v1 <- rnorm(30, 0, 1)
#' buildtesting$v2 <- rnorm(30, 0, 1)
#' buildtesting$v3 <- rnorm(30, 0, 1)
#' buildtesting$v4 <- rnorm(30, 0, 1)
#'
#' predweights(dataset= buildtesting, variables = c("v1", "v2", "v3", "v4"),
#'  id = "ID2", type = "group", gtvar = "group")
#'
#' ID2 <- c(rep(1:15,2))
#' buildtesting2 <- as.data.frame(ID2)
#' buildtesting2$time2 <- c(rep(0,15), rep(12,15))
#' buildtesting2$v1 <- rnorm(30, 0, 1)
#' buildtesting2$v2 <- rnorm(30, 0, 1)
#' buildtesting2$v3 <- rnorm(30, 0, 1)
#' buildtesting2$v4 <- rnorm(30, 0, 1)
#'
#' predweights(dataset= buildtesting2, variables = c("v1", "v2", "v3", "v4"),
#'  id = "ID2", type = "prepost", gtvar = "time2")
#'


predweights <- function(dataset, variables, id, type = "group", pre=NULL, post=NULL, gtvar, cor = "pearson"){

    if(type == "group")
  {
    endpoints <- dataset[, c(variables)]
    samplec <- cor(endpoints, method = cor)
    weights <- 1/rowSums(samplec^2)

  }else if( type == "prepost"){
    
    # BELOW IS THE ORIGINAL CODE WITH DPLYR 

    # timevard <- dataset[,gtvar]
    #
    # if (!is.numeric(timevard) )
    # {
    #   stop("Provide a numeric 'gtvar' to indicate pre and post observations.")
    #
    # }
    # if( length(unique(timevard)) > 2 ){
    #
    #   stop("Only pre-post data is supported with two unique time values.")
    # }
    #
    # varlist <- paste0("diff.", variables)
    #
    # mutatefunc <- function (dataset,id,column, newname) (
    #   dataset %>%
    #     dplyr::group_by(!!as.name(id) ) %>%
    #     dplyr::mutate(!!as.name(newname) := !!as.name(column) - first(!!as.name(column)))
    #
    # )
    #
    # for ( i in 1:length(variables))
    # {
    #   vars <- variables[i]
    #   flist <- varlist[i]
    #   dataset <- mutatefunc(dataset, id,  column = vars, newname = flist)
    # }
    # datadiffs <- dataset[dataset[,(gtvar)] == unique(timevard)[2],]


    ### this is repetitive code and needs to be imported

    filter_by_time_var <- function(df, id, time_var, pre, post, vars){
      # Separates the pre and post results into separate dfs
      grp_pre <- df[df[time_var] == pre, ]
      grp_post <- df[df[time_var] == post, ]
      # Place in ascending order, so that the ids are in the same row
      # COULD USE SOME ERROR HANDLING HERE TO MAKE SURE THE IDS ARE IDENTICAL... do unit test
      sorted_grp_pre <- grp_pre[order(grp_pre[[id]]), ]
      sorted_grp_post <- grp_post[order(grp_post[[id]]), ]
      # Filters such that only the chosen variables in vars are present
      # in each data frame
      pre_vars <- sorted_grp_pre[vars]
      post_vars <- sorted_grp_post[vars]
      return(list(pre_vars = pre_vars, post_vars = post_vars))
    }

    # takes either the mean or median of each variable in two data frames
    # returns a vector of the difference between the two.
    # location of grp_2_data - location of grp_1_data
    
    # not sure if needed, assumes location 
    create_difference_vector <- function(grp_1_data, grp_2_data, location){
      if (location == 'mean') {
        grp_1_means <- colMeans(grp_1_data)
        grp_2_means <- colMeans(grp_2_data)
        differences <- grp_2_means - grp_1_means
        return(unname(as.vector(differences)))
      } else if (location == 'median') {
        grp_1_medians <- apply(grp_1_data, 2, median)
        grp_2_medians <- apply(grp_2_data, 2, median)
        differences <- grp_2_medians - grp_1_medians
        return(unname(as.vector(differences)))
      }
    }
    
    if (type == "prepost") {
      dfs = filter_by_time_var(dataset, id, gtvar, pre, post, variables)
      df_a <- dfs$pre_vars
      df_b <- dfs$post_vars
    }
    
    # subtracting df_a from df_b, but not their locations 
    # if location matters then we can use the create_differnce_vector 
    difference = df_b - df_a

    # different 
    samplec <- cor( difference, method = cor)

    weights <- 1/rowSums(samplec^2)

  }

  outweight <- list(weights)
  return(outweight)
}

