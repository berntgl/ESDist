#' Calculate percentiles with bootstrapped 95% CI.
#'
#' @param df A dataframe.
#' @param es The effect size column.
#' @param se The standard error column (defaults to NULL).
#' @param probs A vector of quantiles ot compute. Given by parent function.
#' @param weighted Defaults to FALSE. If set to TRUE, will weight all effect
#' sizes by their standard error (requires `se` to be defined).
#' @param abs Defaults to FALSE. When set to TRUE, will calculate benchmarks on
#' absolute effect sizes only.
#' @param bowley Defaults to FALSE. Will calculate Bowley's skewness when set
#' to TRUE.
#' @param n_bootstrap Number of bootstrapped samples for benchmark 95% CIs.
#' @param ndec Number of decimals. Defaults to 2.
#'
#' @returns A table.
#' @noRd
#'
calculate_percentiles_ci <- function(df,
                                     es,
                                     se,
                                     probs,
                                     weighted = FALSE,
                                     abs = FALSE,
                                     bowley = FALSE,
                                     n_bootstrap = 1000,
                                     ndec = 2) {
  # Extract the effect sizes
  # Attempt to access the specified column
  if (abs) {
    es_col <- abs(df[[es]])
    df[[es]] <- abs(df[[es]])
  } else {
    es_col <- df[[es]]
  }


  if (weighted) {
    stopifnot(!missing(se))
    se_col <- df[[se]]
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
  percentiles_estimates <- round(apply(results$t, 2, quantile, probs = c(0.025, 0.5, 0.975)), ndec)

  # Calculate mean of the bootstrapped quantiles
  boot_mean_estimates <- round(colMeans(results$t), ndec)

  # Get original percentiles
  original_percentiles <- round(wtd.quantile(es_col, probs = probs,
                                           weights = weights, na.rm = TRUE), ndec)

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

    results_table[paste0(as.integer(probs[i] * 100), "% CIL")] <- percentiles_estimates[1, i]
    #results_table[paste0(as.integer(probs[i] * 100), "_CI_Median")] <- percentiles_estimates[2, i]
    results_table[paste0(as.integer(probs[i] * 100), "% CIU")] <- percentiles_estimates[3, i]
  }
  results_table <- as.data.frame(t(results_table))
  results_table$n <- as.integer(length(es_col))
  if (bowley) {
    Q1 <- original_percentiles[probs == 0.25]
    Q2 <- original_percentiles[probs == 0.50]
    Q3 <- original_percentiles[probs == 0.75]

    denom <- (Q3 - Q1)
    if (denom == 0) {
      results_table$Bowley <- NA_real_
      warning("Cannot compute Bowley's skewness: Q3 equals Q1 (denominator is zero).")
    } else {
      results_table$Bowley <- round((Q3 + Q1 - 2 * Q2) / denom, ndec)
    }
  }
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
#' @param abs Defaults to FALSE. When set to TRUE, will calculate benchmarks on
#' absolute effect sizes only.
#' @param bowley Defaults to FALSE. Will calculate Bowley's skewness when set
#' to TRUE.
#' @param ndec Number of decimals. Defaults to 2.
#'
#' @returns A table.
#' @noRd

calculate_percentiles <- function(df,
                                  es,
                                  se = NULL,
                                  weighted = FALSE,
                                  probs,
                                  abs = FALSE,
                                  bowley = FALSE,
                                  ndec = 2) {

  if (abs) {
    es_col <- abs(df[[es]])
    df[[es]] <- abs(df[[es]])
  } else {
    es_col <- df[[es]]
  }


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
                                    weights = weights, na.rm = TRUE), ndec)

  # Turn into dataframe with each percentile as a column
  percentiles <- as.data.frame(t(percentiles))

  # Add no. of observations
  percentiles$n <- as.integer(length(es_col))

  if (bowley) {
    Q1 <- unname(percentiles[[1]])
    Q2 <- unname(percentiles[[2]])
    Q3 <- unname(percentiles[[3]])

    denom <- (Q3 - Q1)
    if (denom == 0) {
      percentiles$Bowley <- NA_real_
      warning("Cannot compute Bowley's skewness: Q3 equals Q1 (denominator is zero).")
    } else {
      percentiles$Bowley <- round((Q3 + Q1 - 2 * Q2) / denom, ndec)
    }
  }
  return(percentiles)
}
