
# import external functions and data
source("custom_functions.R")
source("example_data.R")


#' Title
#'
#' @param dataset Data to be used
#' @param id A variable indicating the id of each subject, not required for type = 'group'.
#' @param direction Direction of prediction across endpoints. increase (all increase), decrease (all decrease), mixed (provide a vector of predictions for each variable). These are calculated as post-pre or larger group value minus smaller (e.g., with groups of 1 and 0, g1-g0)
#' @param bound Whether or not a bound will be used
#' @param variables Endpoints of interest
#' @param type Type of analysis, pre-post or group
#' @param gtvar Variable denoting either the group or the time, dependent on type
#' @param phi_0 The null hypothesized value
#' @param predictions A matrix with two columns. First column provides the variable names, second column the prediction (increase, decrease or difference).
#' @param location Measure of central tendency, mean or median
#'
#' @return A list of two elements. The first element is an indicator for whether each prediction on a variable was correct, the second element is the observed difference between groups or pre-post.
#' @export
#' @importFrom stats wilcox.test qnorm
#' @examples


# the start of the predresults function
predresults <- function(dataset,
                        id,
                        direction,
                        bound = "wilcoxon",
                        variables,
                        type = "group",
                        gtvar,
                        a,
                        b,
                        phi_0 = 0.50,
                        predictions,
                        location = "median") {

  # error handling
  if (!is.numeric(dataset[, gtvar])) {
    stop(
      "gtvar must be numeric, either a numeric variable for time or group (e.g., group 1 vs group 0)."
    )
  }

  # assigns the filtered data frames to the variables df_a or df_b, based on type
  if (type == "group") {
    dfs = filter_by_group_var(dataset, gtvar, a, b, variables)
    df_a <- dfs$grp_1_vars
    df_b <- dfs$grp_2_vars
  } else if (type == "prepost") {
    dfs = filter_by_time_var(dataset, id, gtvar, a, b, variables)
    df_a <- dfs$pre_vars
    df_b <- dfs$post_vars
  }

  direction_vector <- rep(direction, length(variables))
  diff_vector <- create_difference_vector(df_a, df_b, location)

  if (!missing(direction)) {
    # Calculate 'results' using 'direction_vector' and 'diff_vector'
    results <- create_results_vector(direction_vector, diff_vector)
  } else if (!missing(predictions)) {
    # Calculate 'results' using 'predictions', 'diff_vector', and other arguments
    results <- create_results_vector(predictions, diff_vector, diff_method = bound, group_a = a, group_b = b, phi_0 = phi_0)
  } else {
    # error handling. Needs more updates. Could be placed at the start of the function
    stop("Either predictions or direction must be filled.")
  }
  # might be unnecessary
  # results <- create_results_vector(direction_vector,diff_vector)
  list_to_plot <- list(results, diff_vector, variables)
  return(list_to_plot)
}
# Call predresults for groups
predresults(dataset = buildtestinggroup, direction="decrease",variables = c("v1","v2", "v3", "v4"), type="group", gtvar="group", a=0, b=12)

# Call predresults for pre/post
predresults(dataset = buildtesting2, id="ID2", direction="increase", variables = c("v1", "v3"), type="prepost", gtvar="time2",a=0, b=12, location="mean")

# call using the difference tests




