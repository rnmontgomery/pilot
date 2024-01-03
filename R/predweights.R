# set working directory 
# setwd('/userInfo')

# import external functions and data
source("custom_functions.R")
source("example_data.R")

# Load the package (if organized as a package)
# devtools::load_all()

# Define the predweights function
#' Calculate weights based on correlation for a given dataset.
#'
#' This function calculates weights based on correlation for either a group or
#' prepost type of dataset.
#'
#' @param dataset The input dataset.
#' @param variables The variables for which to calculate weights.
#' @param type The type of dataset ('group' or 'prepost').
#' @param id The identifier variable (used in prepost type).
#' @param pre A vector specifying the pre-observation time (used in prepost type).
#' @param post A vector specifying the post-observation time (used in prepost type).
#' @param gtvar The grouping or time variable.
#' @param corr_method The method for calculating correlation.
#'
#' @return A list containing calculated weights.
#'
#' @examples
#' # Example for "group" type
#' predweights(dataset = buildtesting, variables = c("v1", "v2", "v3", "v4"), id = "ID2", type = "group", gtvar = "group")
#'
#' # Example for "prepost" type
#' predweights(dataset = buildtesting2, variables = c("v1", "v2", "v3", "v4"), id = "ID2", type = "prepost", pre = 0, post = 12, gtvar = "time2")
#'
#' # Additional examples if needed...

#'
#' @export
predweights <- function(dataset, variables, type = "group", id=NULL, pre=NULL, post=NULL, gtvar, corr_method = "pearson") {

  if (type == "group") {
    # read the article again
    # learn what's happening here
    endpoints <- dataset[, variables]
    samplec <- cor(endpoints, method = corr_method)
  } else if (type == "prepost") {
    dfs <- filter_by_time_var(dataset, id, gtvar, pre, post, variables)
    df_a <- dfs$pre_vars
    df_b <- dfs$post_vars
    pre_post_difference <- df_b - df_a
    samplec <- cor(pre_post_difference, method = corr_method)
  }

  weights <- 1/rowSums(samplec^2)
  outweight <- list(weights)
  return(outweight)
}

