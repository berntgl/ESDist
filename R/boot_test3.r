# Load required libraries
library(Hmisc)
library(boot)
library(rlang)
library(dplyr)

library(boot)
library(rlang)


# Function to calculate quartiles with bootstrapped confidence intervals
calculate_percentiles_ci <- function(data, es_col_name, se_col_name, probs, n_bootstrap = n_bootstrap) {
  # Extract the effect sizes
  es_col <- data[[es_col_name]]
  se_col <- data[[se_col_name]]

  # Calculate the weights using the inverse of the variance (1 / (se_col^2))
  weights <- 1 / (se_col^2)

  # Normal bootstrap function
  boot_function <- function(data, indices) {
    sampled_es_col <- es_col[indices]  # Resample effect sizes
    sampled_weights <- weights[indices]  # Resample weights
    # Calculate quartiles on the resampled data
    quartiles <- wtd.quantile(sampled_es_col, probs = probs,
                              weights = sampled_weights, na.rm = TRUE)
    return(quartiles)
  }

  # Prepare data for bootstrapping
  data_for_boot <- data.frame(es_col = es_col)

  # Perform bootstrapping
  results <- boot(data = data_for_boot, statistic = boot_function,
                  R = n_bootstrap)

  # Calculate the bootstrapped quartiles
  quartiles_estimates <- round(apply(results$t, 2, quantile, probs = c(0.025, 0.5, 0.975)), 2)

  # Get original quartiles
  original_quartiles <- round(wtd.quantile(es_col, probs = probs,
                                           weights = weights, na.rm = TRUE), 2)

  # Create results table with dynamic column names
  results_table <- c()

  # Fill the results table with original quartiles and bootstrapped confidence intervals
  for (i in seq_along(probs)) {
    # Multiply by 100 and convert to integer for the probability names
    quantile_name <- paste0(as.integer(probs[i] * 100), "_Original")
    results_table[quantile_name] <- original_quartiles[i]

    results_table[paste0(as.integer(probs[i] * 100), "_CI_Lower")] <- quartiles_estimates[1, i]
    results_table[paste0(as.integer(probs[i] * 100), "_CI_Median")] <- quartiles_estimates[2, i]
    results_table[paste0(as.integer(probs[i] * 100), "_CI_Upper")] <- quartiles_estimates[3, i]
  }

  return(results_table)
}

# Function for calculating percentiles
calculate_percentiles <- function(data, es_col_name, se_col_name, probs) {

  es_col <- data[[es_col_name]]
  se_col <- data[[se_col_name]]

  # Calculate the weights using the inverse of the variance (1 / (se_col^2))
  weights <- 1 / (se_col^2)

  # Calculate the percentiles, round to no. specified decimals
  percentiles <- round(wtd.quantile(es_col, probs = probs,
                                    weights = weights, na.rm = TRUE), 2)

  # Turn into dataframe with each percentile as a column
  percentiles <- as.data.frame(t(percentiles))

  # Add no. of observations
  percentiles$n <- length(es_col)

  return(percentiles)
}

#####

esd_table_test <- function(data, es_col_name, se_col_name, grouping_var = NULL, min_group_size = 3, ci = FALSE, method = "quads", n_bootstrap = 1000) {

  # Define the probs based on the specified method
  if (method == "quads") {
    probs <- c(0.25, 0.5, 0.75)
  } else if (method == "thirds") {
    probs <- c(0.1665, 0.5, 0.8335)
  }

  # Condition on a grouping var being specified
  if (!is.null(grouping_var)) {

    # Initialise a results list
    results <- list()

    # Iterate over each unique group in the grouping variables
    for (group in unique(data[[deparse(substitute(grouping_var))]])) {

      # Create group-specific data
      group_data <- data[data[[deparse(substitute(grouping_var))]] == group, ]

      # Calculate percentiles for group
      if (nrow(group_data) > min_group_size) {
        if (ci) {
          results[[group]] <- calculate_percentiles_ci(data = group_data,
                                                       es_col_name = deparse(substitute(es_col_name)),
                                                       se_col_name = deparse(substitute(se_col_name)),
                                                       probs = probs,
                                                       n_bootstrap = n_bootstrap)
        } else {
          results[[group]] <- calculate_percentiles(data = group_data,
                                                    es_col_name = deparse(substitute(es_col_name)),
                                                    se_col_name = deparse(substitute(se_col_name)),
                                                    probs = probs)
        }
      }
    }

    # Calculate overall percentiles
    if (ci) {
      results[["All"]] <- calculate_percentiles_ci(data = data,
                                                   es_col_name = deparse(substitute(es_col_name)),
                                                   se_col_name = deparse(substitute(se_col_name)),
                                                   probs = probs,
                                                   n_bootstrap = n_bootstrap)

    } else {
      results[["All"]] <- calculate_percentiles(data = data,
                                                es_col_name = deparse(substitute(es_col_name)),
                                                se_col_name = deparse(substitute(se_col_name)),
                                                probs = probs)

    }

    # Combine results in a dataframe
    results <- bind_rows(results, .id = "Group")

  } else {
    results <- calculate_percentiles(data = data,
                                     es_col_name = deparse(substitute(es_col_name)),
                                     se_col_name = deparse(substitute(se_col_name)),
                                     probs = probs)
  }

  return(as.data.frame(results))
}

# Example of how to call your function
# esd_table_test(ot_dat, es_col_name = yi, se_col_name = sei, grouping_var = group, ci = TRUE)

