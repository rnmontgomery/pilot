# set up to run unit tests
# library(testthat)
# devtools::load_all()
# usethis::use_testthat(3)
#

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


# predtests

# currently broken
predtest_bootstrap = function(weights, results, phi_0=.5){
  teststat <- weights%*%results
  ntests <- length(weights)
  correct <- sum(results)

  boots <- matrix(NA,replicates,1)
  for (g in 1:replicates)
  {
    boots[g,] <- ifelse((rbinom(ntests,1,nullphi))%*%weights >= teststat,1,0)
  }
  pval <- mean(boots)
  return(pval)
}

predtest_approx = function(weights, results,phi_0=.5){
  test_stat = weights %*% results
  sum_of_weights = sum(weights)
  squares = weights^2
  mu = phi_0 * sum_of_weights
  sigma = sqrt(phi_0 * (1 - phi_0) * sum(squares))
  z_score = (test_stat - mu) / sigma
  approx_p_val = pnorm(z_score, lower.tail = FALSE)
  return(approx_p_val)
}

predtest_exact = function(weights, results,phi_0=.5) {

  teststat = weights %*% results
  ntests <- length(weights)
  nperm <- 2^ntests
  perms <- as.matrix(expand.grid(rep(list(0:1), ntests)))
  values <- perms%*%as.matrix(weights)
  rank <- as.data.frame(cbind(values,rank(values)))
  # pval <- 1-(rank[which(rank$V1 == as.numeric(teststat)),2]/nperm)
  pval <- dim(rank[rank$V2>= rank[which(rank$V1 == as.numeric(teststat)),2],])[1]/nperm
  return(pval)

}

