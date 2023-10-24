
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

# example data frame for analysis on group data
ID2 <- 1:30
buildtestinggroup <- as.data.frame(ID2)
buildtestinggroup$group <- c(rep(0,15), rep(12,15))
buildtestinggroup$sex <- rbinom(30,1,.5)
buildtestinggroup$v1 <- rnorm(30, 0, 1)
buildtestinggroup$v2 <- rnorm(30, 0, 1)
buildtestinggroup$v3 <- rnorm(30, 0, 1)
buildtestinggroup$v4 <- rnorm(30, 0, 1)

buildtestinggroup

# example dataframe for analysis on pre/post data
ID2 <- c(rep(1:15,2))
buildtesting2 <- as.data.frame(ID2)
buildtesting2$time2 <- c(rep(0,15), rep(12,15))
buildtesting2$v1 <- rnorm(30, 0, 1)
buildtesting2$v2 <- rnorm(30, 0, 1)
buildtesting2$v3 <- rnorm(30, 0, 1)
buildtesting2$v4 <- rnorm(30, 0, 1)


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

  # NESTED FUNCTIONS

  # The following functions filter the dataset into two groups
  filter_by_group_var <- function(df, grp_var, grp_1, grp_2, vars){
    # Subset the data by group with only the column variables mentioned in ...
    grp_1_vars <- df[df[[grp_var]] == grp_1, vars]
    grp_2_vars <- df[df[[grp_var]] == grp_2, vars]
    # place the data frames into a list and return it
    return(list(grp_1_vars = grp_1_vars, grp_2_vars = grp_2_vars))
  }

  filter_by_time_var <- function(df, id, time_var, pre, post, vars){
    # Separates the pre and post results into separate dfs
    grp_pre <- df[df[time_var] == pre, ]
    grp_post <- df[df[time_var] == post, ]
    # Place in ascending order, so that the ids are in the same row
    # COULD USE SOME ERROR HANDLING HERE TO MAKE SURE THE IDS ARE IDENTICAL
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

  # function to obtain the results
  # will have added feature for mixed
  create_results_vector <- function(hypothesis, results, diff_method = 'wilcoxon', group_a = NULL, group_b = NULL, phi_0 = .5) {
    # error handling
    if (length(hypothesis) != length(results)) stop('hypothesis and results need to be the same length')
    if (class(results) != 'numeric') stop('results need to be numeric data type')
    valid_hypotheses = c('increase', 'decrease', 'difference')# Valid prediction values
    if (any(!hypothesis %in% valid_hypotheses)) {
      stop('Invalid hypothesis detected. Valid hypotheses are: ', paste(valid_hypotheses, collapse = ', '))
    }
    # methods of obtaining p values
    wilcoxon_test_p_val <- function(a, b, phi_0) wilcox.test(a, b, mu = phi_0)$p.value
    t_test_p_val <- function(a, b) t.test(a, b)$p.value

    # assignment of the user's chosen difference method to get a p value
    p_val_test <- switch(diff_method, wilcox = wilcoxon, t = t_test)

    # empty vector that will take 1s and 0s to determine accuracy of predictions
    accuracy <- numeric(length(hypothesis))

    for (i in seq_along(hypothesis)) {
      # difference means that the user predicted that by the chosen test
      # that there'd be a statistically significant difference for a particular variable
      if (hypothesis[i] == 'difference') {
        # locates the ith col variable of group_a and group_b
        col_var_a <- group_a[[i]]
        col_var_b <- group_b[[i]]
        # assigns the p value from the chosen test
        p_val = p_val_test(col_var_a, col_var_b, phi_0)
        accuracy[i] <- ifelse(p_val < phi_0, 1, 0)
      } else if ((hypothesis[i] == 'increase' && results[i] >= 0) || (hypothesis[i] == 'decrease' && results[i] < 0)) {
        accuracy[i] <- 1
      } else {
        accuracy[i] <- 0
      }
    }

    return(accuracy)
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
  results <- create_results_vector(direction_vector,diff_vector)
  list_to_plot <- list(results, diff_vector, variables)
  return(list_to_plot)
}
# Call predresults for groups
predresults(dataset = buildtestinggroup, direction="decrease",variables = c("v1","v2", "v3", "v4"), type="group", gtvar="group", a=0, b=12)

# Call predresults for pre/post
predresults(dataset = buildtesting2, id="ID2", direction="increase", variables = c("v1", "v3"), type="prepost", gtvar="time2",a=0, b=12, location="mean")

# call using the difference tests




