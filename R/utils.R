#' Calculate percentiles with bootstrapped 95% CI.
#'
#' @param df A dataframe.
#' @param es The effect size column.
#' @param se The standard error column (defaults to NULL).
#' @param probs A vector of quantiles ot compute. Given by parent function.
#' @param weighted Defaults to FALSE. If set to TRUE, will weight all effect
#' sizes by their standard error (requires `se` to be defined).
#' @param n_bootstrap Number of bootstrapped samples for benchmark 95% CIs.
#'
#' @returns A table.
#' @noRd
#'
calculate_percentiles_ci <- function(df,
                                     es,
                                     se,
                                     probs,
                                     weighted,
                                     n_bootstrap) {
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
    quantile_name <- paste0(as.integer(probs[i] * 100), "%")
    results_table[quantile_name] <- original_percentiles[i]

    # Add mean of bootstrapped quantiles
    #boot_mean_name <- paste0(as.integer(probs[i] * 100), "_Boot_Mean")
    #results_table[boot_mean_name] <- boot_mean_estimates[i]

    results_table[paste0(as.integer(probs[i] * 100), "% CI Lower")] <- percentiles_estimates[1, i]
    #results_table[paste0(as.integer(probs[i] * 100), "_CI_Median")] <- percentiles_estimates[2, i]
    results_table[paste0(as.integer(probs[i] * 100), "% CI Upper")] <- percentiles_estimates[3, i]
  }
  results_table <- as.data.frame(t(results_table))
  results_table$n <- as.integer(length(es_col))
  return(results_table)
}

#' Calculate percentiles.
#'
#' @param df A dataframe.
#' @param es The effect size column.
#' @param se The standard error column (defaults to NULL).
#' @param weighted Defaults to FALSE. If set to TRUE, will weight all effect
#' sizes by their standard error (requires `se` to be defined).
#' @param probs A vector of quantiles ot compute. Given by parent function.
#'
#' @returns A table.
#' @noRd

calculate_percentiles <- function(df, es, se = NULL, weighted = FALSE, probs) {

  es_col <- df[[es]]

  if (weighted) {
    stopifnot(!missing(se))

    se_col <- df[[se]]
    # Calculate the weights using the inverse of the variance (1 / (se_col^2))
    weights <- 1 / (se_col^2)
  } else {
    weights <- rep(1, length(es_col))
  }

  # Calculate the percentiles, round to no. specified decimals
  percentiles <- round(wtd.quantile(es_col, probs = probs,
                                    weights = weights, na.rm = TRUE), 2)

  # Turn into dataframe with each percentile as a column
  percentiles <- as.data.frame(t(percentiles))

  # Add no. of observations
  percentiles$n <- as.integer(length(es_col))

  return(percentiles)
}
