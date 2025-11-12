
#' Calculating percentiles with 95% CIs based on bootstrapping
#'
#' @param df A dataframe.
#' @param es A string with the name of the effect sizes column.
#' @param se A string with the name of the std errors column.
#' @param probs A vector of percentiles to compute. Will be automatically
#' given by the esd_* functions based on the pre-specified ```method```
#' argument.
#' @param weightedAn argument which defaults to FALSE in the esd_* functions
#' indicating whether the distribution should be weighted by the inverse
#' variance.
#' @param n_bootstrap Number of bootstrapped samples. Will be automatically
#' given by the esd_* functions, but can be changed by the user.
#' @keywords internal
#' @returns A row containing the original percentiles and the bootstrapped
#' percentiles (mean, median, and 95% CI for each percentile)
#'
#' @examples calculate_percentiles_ci(ot_dat, yi, sei, weighted, probs)
#'
calculate_percentiles_ci <- function(df, es, se, probs, weighted, n_bootstrap) {
  # Extract the effect sizes
  # Attempt to access the specified column
  es_col <- df[[es]]
  se_col <- df[[se]]

  if (weighted) {
    stopifnot(!missing(se))
    weights <- 1 / (se_col^2)
  } else {
    weights <- rep(1, length(es_col))
  }

  # Prepare df for bootstrapping
  data_for_boot <- data.frame(es_col = es_col)

  bootstrap_quantiles <- function(df, indices) {
    sampled_es_col <- es_col[indices]  # Resample effect sizes
    sampled_weights <- weights[indices]  # Resample weights
    # Calculate percentiles on the resampled df
    percentiles <- wtd.quantile(sampled_es_col, probs = probs,
                              weights = sampled_weights, na.rm = TRUE)
    return(percentiles)
  }

  # Perform bootstrapping
  results <- boot(data = data_for_boot, statistic = bootstrap_quantiles,
                  R = n_bootstrap)

  # Calculate the bootstrapped percentiles
  percentiles_estimates <- round(apply(results$t, 2, quantile, probs = c(0.025, 0.5, 0.975)), 2)

  # Calculate mean of the bootstrapped quantiles
  boot_mean_estimates <- round(colMeans(results$t), 2)

  # Get original percentiles
  original_percentiles <- round(wtd.quantile(es_col, probs = probs,
                                           weights = weights, na.rm = TRUE), 2)

  # Create results table with dynamic column names
  results_table <- c()

  # Fill the results table with original percentiles, mean of bootstrapped quantiles,
  # and bootstrapped confidence intervals
  for (i in seq_along(probs)) {
    # Multiply by 100 and convert to integer for the probability names
    quantile_name <- paste0(as.integer(probs[i] * 100), "_Original")
    results_table[quantile_name] <- original_percentiles[i]

    # Add mean of bootstrapped quantiles
    boot_mean_name <- paste0(as.integer(probs[i] * 100), "_Boot_Mean")
    results_table[boot_mean_name] <- boot_mean_estimates[i]

    results_table[paste0(as.integer(probs[i] * 100), "_CI_Lower")] <- percentiles_estimates[1, i]
    results_table[paste0(as.integer(probs[i] * 100), "_CI_Median")] <- percentiles_estimates[2, i]
    results_table[paste0(as.integer(probs[i] * 100), "_CI_Upper")] <- percentiles_estimates[3, i]
  }
  results_table["n"] <- length(es_col)
  return(results_table)
}

#' Calculating percentiles
#'
#' @param df A dataframe.
#' @param es A string with the name of the effect sizes column.
#' @param se A string with the name of the std errors column.
#' @param weighted An argument which defaults to FALSE in the esd_* functions
#' indicating whether the distribution should be weighted by the inverse
#' variance.
#' @param probs A vector of percentiles to compute. Will be automatically
#' provided by the esd_* functions based on the pre-specified ```method```
#' argument.
#' @keywords internal
#' @returns A single row of
#'
#' @examples calculate_percentiles(ot_dat, yi, sei, weighted, probs)

calculate_percentiles <- function(df, es, se, weighted, probs) {

  es_col <- df[[es]]
  se_col <- df[[se]]

  if (weighted) {
    stopifnot(!missing(se))

    # Calculate the weights using the inverse of the variance (1 / (se_col^2))
    weights <- 1 / (se_col^2)
  } else {
    weights <- 1
  }

  # Calculate the percentiles, round to no. specified decimals
  percentiles <- round(wtd.quantile(es_col, probs = probs,
                                    weights = weights, na.rm = TRUE), 2)

  # Turn into dataframe with each percentile as a column
  percentiles <- as.data.frame(t(percentiles))

  # Add no. of observations
  percentiles$n <- length(es_col)

  return(percentiles)
}
