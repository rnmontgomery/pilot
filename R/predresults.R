# Set working directory
setwd('/userInfo')

# import external functions and data
source("custom_functions.R")
source("example_data.R")

#' @param dataset The input dataset.
#' @param id Identifier variable for pre/post type. Default is NULL.
#' @param hypothesis The hypotheses to test. Can be a string or a vector of strings, including "increase", "decrease", or "different".
#' @param bound The method for testing differences. Options are "wilcoxon" or "t". Default is "wilcoxon".
#' @param variables The variables on which the hypotheses are based.
#' @param type The type of analysis. Options are "group" or "prepost". Default is "group".
#' @param gtvar The grouping or time variable for analysis.
#' @param a The value for the first group or the starting time point for pre/post type. Default is NULL.
#' @param b The value for the second group or the ending time point for pre/post type. Default is NULL.
#' @param phi_0 The null hypothesis threshold. Default is 0.50.
#' @param location The location measure for creating difference vector. Options are "mean" or "median". Default is "median".
#'
#' @return A list containing the predicted results, difference vector, and the variables.
#'

#' @examples
#' # Call predresults for groups
#predresults(dataset = buildtesting, hypothesis="decrease",variables = c("v1","v2", "v3", "v4"), type="group", gtvar="group", a=0, b=1)
# Call predresults for pre/post
#predresults(dataset = buildtesting2, id="ID2", hypothesis="increase", variables = c("v1", "v3"), type="prepost", gtvar="time2",a=0, b=12, location="mean")
# call using the difference tests
#predresults(dataset = buildtesting2, id="ID2", hypothesis=c("increase","different"), variables = c("v1", "v3"), type="prepost", gtvar="time2",a=0, b=12, location="mean")

#'@export

predresults <- function(dataset,
                        id=NULL,
                        hypothesis,
                        bound = "wilcoxon",
                        variables,
                        type = "group",
                        gtvar,
                        a=NULL,
                        b=NULL,
                        phi_0 = 0.50,
                        location = "median") {

  # error handling
  if (!is.numeric(dataset[, gtvar])) {
    stop(
      "gtvar must be numeric, either a numeric variable for time or group (e.g., group 1 vs group 0)."
    )
  }

  # assigns the filtered data frames to the variables df_a or df_b,
  # based on if type is "group" or "prepost"
  if (type == "group") {
    dfs = filter_by_group_var(dataset, gtvar, a, b, variables)
    df_a <- dfs$grp_1_vars
    df_b <- dfs$grp_2_vars
  } else if (type == "prepost") {
    dfs = filter_by_time_var(dataset, id, gtvar, a, b, variables)
    df_a <- dfs$pre_vars
    df_b <- dfs$post_vars
  }

  diff_vector <- create_difference_vector(df_a, df_b, location) # vector of the differences between groups
  # a vector of 1s (correctly predicted) and 0s (incorrectly predicted)
  results = get_results_vector(hypothesis, variables, diff_vector, diff_method=bound, grp_a=df_a, grp_b=df_b) # grp_a/b don't need to be defined here, but it doesn't do anything if they are

  list_to_plot <- list(results, diff_vector, variables) #col vars with desired for each one
  return(list_to_plot)
}
