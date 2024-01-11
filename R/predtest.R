# Set working directory
setwd('/Users/richardvargas/Documents/R/gra_work/pilot_local/R')

# Import custom functions
source("custom_functions.R")

# Import example data
source("example_data.R")

#' Perform a prediction test based on specified parameters.
#'
#' This function conditionally calls the appropriate helper function based on the specified test type.
#'
#' @param weights_vector A vector of weights.
#' @param results_vector A vector of results.
#' @param test_type The type of test to perform ("exact", "approx", or "bootstrap").
#' @param phi_0 The null hypothesis value for the test.
#'
#' @return The p-value resulting from the prediction test.
#'
#' @examples
#' # Example usage for "approx" test
#'predtest(big_weights, big_results, test_type = "approx", phi_0 = 0.5)
#'predtest(weights,results, test_type="exact", phi_0=.5)
#'
#' # Additional examples for other test types if needed...
#'
#' @export
predtest <- function(weights_vector, results_vector, test_type, phi_0 = 0.5) {
  # control flow
  # conditionally calls the appropriate helper function
  #   - eligible options are in the options vector
  # assigns value to p_val
  # returns a list of specific data
  options <- c("exact", "approx", "bootstrap")

  if (test_type == "exact") {
    value_list <- predtest_exact(weights_vector, results_vector, phi_0)
  } else if (test_type == "approx") {
    value_list <- predtest_approx(weights_vector, results_vector, phi_0)
  } else if (test_type == "bootstrap") {
    value_list <- predtest_bootstrap(weights_vector, results_vector, phi_0) # broken: needing to develop this with Dr. Montgomery
  }

  return(value_list)
}

