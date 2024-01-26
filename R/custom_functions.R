# set up to run unit tests
# library(testthat)
# devtools::load_all()
# usethis::use_testthat(3)


# purely for testing connection
add_one = function(n){
  return(n + 1)
}

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
create_difference_vector <- function(grp_1_data, grp_2_data, location='median'){
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
get_results_vector <- function(hypothesis, vars, differences, diff_method = 'wilcoxon', grp_a = NULL, grp_b = NULL, phi_0=0.5) {
  # error handling

  if (!all(sapply(differences, function(x) is.numeric(x)))) {
    stop("differences must only contain numeric values")
  }

  valid_hypotheses <- c('decrease', 'increase', 'different')

  # if hypothesis is a string it becomes a vector of length(vars) of a valid hypothesis
  if (is.character(hypothesis) && length(hypothesis) == 1) {
    hypothesis_vector <- switch(
      hypothesis[[1]],  # Access the first element of the hypothesis vector
      increase = rep('increase', length(vars)),
      decrease = rep('decrease', length(vars)),
      different = rep('different', length(vars)),
      stop("Invalid hypothesis. Choose from a string or vector containing: 'decrease', 'increase', 'different'")
    )
  } else if (is.vector(hypothesis) && all(hypothesis %in% valid_hypotheses)) {
    hypothesis_vector <- hypothesis
  } else {
    stop("Invalid hypothesis. It must be either a string or a vector of the strings: 'increase', 'decrease', 'different'.")
  }
  # confirm same sized vectors for differences, vars, and hypothesis (no longer be a string)
  if (length(differences) != length(hypothesis_vector)) stop("differences, vars, and  hypothesis must be same length vectors, unless hypothesis is a string")
  if (length(differences) != length(vars)) stop("differences, vars, and  hypothesis must be same length vectors, unless hypothesis is a string")

  # error handling cases of handling 'different' as a hypothesis

  # empty stack to eventually return results
  results <- numeric(length(differences))

  # Declare n as the length of vars
  n <- length(vars)

  for (i in 1:n) {
    if (hypothesis_vector[i] == 'increase' && differences[i] > 0) {
      results[i] <- 1
    } else if (hypothesis_vector[i] == 'decrease' && differences[i] < 0) {
      results[i] <- 1
    } else if (hypothesis_vector[i] == 'different') {
      # might need to create a new function to subset the variable columns to do the tests on
      if (diff_method == 'wilcoxon') {
        # do wilcox.test on vars[i] in grp_a and grp_b
        test_result <- wilcox.test(grp_a[[vars[i]]], grp_b[[vars[i]]], paired = TRUE, alternative = "two.sided")
      } else if (diff_method == 't') {
        # do t.test on vars[i] in grp_a and grp_b
        test_result <- t.test(grp_a[[vars[i]]], grp_b[[vars[i]]], paired = TRUE)
      }
      if (test_result$p.value < phi_0){
        results[i] <- 1
      }
    } else {
      results[i] <- 0
    }
  }

  return(results)
}

# predtests

# updated. needs to be checked by Dr. Montgomery

predtest_bootstrap <- function(weights, results, nullphi = 0.50, sims = 5000){

  test_stat <- weights%*%results
  ntests <- length(weights)
  correct <- sum(results)

  if (length(nullphi) == 1 | length(nullphi) == ntests)
  {
    boots <- matrix(NA,sims,1)
    for (g in 1:sims)
    {
      boots[g,] <- ifelse((rbinom(ntests,1,nullphi))%*%weights >= test_stat,1,0)
    }
    p_val <- mean(boots)
    value_list <- list(
      test_stat = test_stat,
      p_value = p_val,
      num_correctly_predicted = sum(results)
    )
    return(value_list)

  } else{
    stop("nullphi needs to be either a single value or specified for every endpoint")
  }

}

predtest_approx = function(weights, results,phi_0=.5){
  test_stat = weights %*% results
  sum_of_weights = sum(weights)
  squares = weights^2
  mu = phi_0 * sum_of_weights
  sigma = sqrt(phi_0 * (1 - phi_0) * sum(squares))
  z_score = (test_stat - mu) / sigma
  approx_p_val = pnorm(z_score, lower.tail = FALSE)
  # list of values of interest
  value_list <- list(
    test_stat = test_stat,
    p_value = approx_p_val,
    num_correctly_predicted = sum(results)
  )
  return(value_list)
}

predtest_exact = function(weights, results, phi_0=.5) {

  test_stat = weights %*% results
  ntests <- length(weights)
  nperm <- 2^ntests
  perms <- as.matrix(expand.grid(rep(list(0:1), ntests)))
  values <- perms%*%as.matrix(weights)
  rank <- as.data.frame(cbind(values,rank(values)))
  p_val <- dim(rank[rank$V2 >= rep(rank$V2[which(rank$V1 == as.numeric(test_stat))[1]], length(rank$V2)), ])[1] / nperm

  value_list <- list(
    test_stat = test_stat,
    p_value = p_val,
    num_correctly_predicted = sum(results)
  )
  return(value_list)

}

