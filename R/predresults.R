
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

  # below are nested functions to be assigned to the difference and results vectors

  # calculates the mean or median difference betwween two specifed groups
  group_diff <- function(df, grp_col, location='median', grp_1, grp_2, variables) {
    # Subset the data by group with only the column variables mentioned in ...
    grp_1_data <- df[df[[grp_col]] == grp_1, variables]
    grp_2_data <- df[df[[grp_col]] == grp_2, variables]

    # calculate either the mean or median for each column variable
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

  # calculates the mean or median difference between two specifed times for each
  # unique id identified by the user 
  pre_post_diff <- function(df, id, time, location='median', pre, post, variables) {

    # Subset the data for pre and post time points
    pre_data <- df[df[[time]] == pre, ]
    post_data <- df[df[[time]] == post, ]

    # Merge pre and post data by the ID column
    merged_data <- merge(pre_data, post_data, by = id, suffixes = c("_pre", "_post"))

    # new data frame for differences
    differences_by_id <- data.frame(
      id = merged_data[[id]]
    )

    # Calculate differences for each specified column
    for (col_name in variables) {
      col_name <- as.character(col_name)
      differences_by_id[[paste0(col_name, "difference")]] <- merged_data[[paste0(col_name, "_post")]] - merged_data[[paste0(col_name, "_pre")]]
    }

    columns_of_interest <- differences_by_id[, -1]
    # calculate either the mean or median for each column variable
    if (location == 'mean') {
      var_means <- colMeans(columns_of_interest)
      return(unname(as.vector(var_means)))
    } else if (location == 'median') {
      var_medians <- apply(columns_of_interest, 2, median)
      return(unname(as.vector(var_medians)))
    }

  }

  # calculates the result vector given the differences 
  create_results_vector <- function(diff_vector, prediction) {

    valid_predictions <- c('increase', 'decrease', 'mixed') # Valid prediction values

    if (!(prediction %in% valid_predictions)) {
      stop("prediction argument needs to be 'increase' or 'decrease'")
    }

    # Calculate results vector based on prediction
    if (prediction == 'increase') {
      results <- as.integer(diff_vector > 0)
    } else if (prediction == 'decrease') {
      results <- as.integer(diff_vector < 0)
    }

    return(results)
  }
  # end of nested functions 
  
  # call nested functions to meet user's request to create differencess and results vectors
  if (type == "group") {
    differences <- group_diff(dataset, gtvar, location, a, b, variables)
  } else if (type == "prepost") {
    differences <- pre_post_diff(dataset, id, gtvar, location, a, b, variables)
  }

  results = create_results_vector(differences, direction)

  list_to_plot <- list(results, differences, variables)
  return(list_to_plot)
}

# Call predresults for groups
predresults(dataset = buildtestinggroup, direction="decrease",variables = c("v1","v2", "v3", "v4"), type="group", gtvar="group", a=0, b=12)

# Call predresults for pre/post
predresults(dataset = buildtesting2, id="ID2", direction="increase", variables = c("v1", "v3"), type="prepost", gtvar="time2",a=0, b=12, location="mean")


# this is the old code for the results vector
# I'm keeping it commented out for further development on the mixed category

# if (direction == "increase") {
#   for (z in 1:length(variables)) {
#     results[c(variables[z])] <-
#       ifelse(results[c(variables[z])] > 0, 1, 0)
#     predictions <- rep(phi_0, length(variables))
#   }
# } else if (direction == "decrease") {
#   for (z in 1:length(variables)) {
#     results[c(variables[z])] <-
#       ifelse(results[c(variables[z])] < 0, 1, 0)
#     predictions <- rep(phi_0, length(variables))
#   } # The if else on 'mixed' needs to be re-examined later
# } else if (direction == "mixed") {
#   for (j in 1:dim(predictions)[1]) {
#     rules <- predictions[predictions[, 1] == names(results[j]), ]
#     if (rules[2] == "increase") {
#       results[j] <- ifelse(results[j] > 0, 1, 0)
#     } else if (rules[2] == "decrease") {
#       results[j] <- ifelse(results[j] < 0, 1, 0)
#     } else if (rules[2] == "difference") {
#       if (bound == "wilcoxon") {
#         split <- dataset[, c(gtvar, names(results[j]))]
#         split1 <-
#           split[split[, gtvar] == as.numeric(as.character(levels[1])), ]
#         split2 <-
#           split[split[, gtvar] == as.numeric(as.character(levels[2])), ]
#         results[j] <-
#           ifelse(wilcox.test(split1[, names(results[j])], split2[, names(results[j])])$p.value <
#                    phi_0,
#                  1,
#                  0)
#       } else if (bound == "normal") {
#         split <- dataset[, c(gtvar, names(results[j]))]
#         split1 <-
#           split[split[, gtvar] == as.numeric(as.character(levels[1])), ]
#         split2 <-
#           split[split[, gtvar] == as.numeric(as.character(levels[2])), ]
#         obdiff <-
#           mean(split1[, names(results[j])]) - mean(split2[, names(results[j])])
#         results[j] <-
#           ifelse(obdiff >  qnorm(1 - (phi_0 / 2)) |
#                    obdiff < qnorm(phi_0 / 2),
#                  1,
#                  0)
#       }
#     }
#   }
# }



