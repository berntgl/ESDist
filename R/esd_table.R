#' Creating a table for field-specific effect size benchmarks
#'
#' @param df dfset.
#' @param es Column name of effect sizes.
#' @param se Column name of standard error.
#' @param weighted Defaults to FALSE. When set to TRUE, will calculate the
#'  weighted distribution based on the inverse standard error.
#' @param grouping_var Column name of grouping variable.
#' @param method Defaults to 'quads', calculating the 25%, 50%, and 75%
#' percentiles. Can also be 'thirds', calculating the 16.65%, 50%, and 83.35%
#' percentiles.
#' @param ci Defaults to "FALSE". When set to TRUE, will calculate bootstrapped
#' estimates of each percentile, along with their 95% CI.
#' @param min_group_size Sets the minimum amount of effect sizes needed to
#' include a group in the table. Defaults to 3.
#' @param csv_write Defaults to FALSE. Will write the outputted table as a csv.
#' @param path_file_name A string containing the directory to which the .csv
#' file will be saved, including the title of the .csv file (has to end in
#' '.csv').
#' @param ndec The number of decimal places in which all values should be
#' reported. Defaults to 2.
#'
#' @return A table.
#' @export
#'
#' @examples esd_table(ot_dat, yi, grouping_var = group, method = "thirds")
#'
#'
esd_table <- function(df,
                      es,
                      se = NULL,
                      weighted = FALSE,
                      grouping_var = NULL,
                      min_group_size = 3,
                      method = "quads",
                      ci = FALSE,
                      n_bootstrap = 1000,
                      csv_write = FALSE,
                      path_file_name = "esd_table.csv",
                      ndec = 2) {


  # Define the probs based on the specified method
  if (method == "quads") {
    probs <- c(0.25, 0.5, 0.75)
  } else if (method == "thirds") {
    probs <- c(0.1665, 0.5, 0.8335)
  }

  # Condition on a grouping var being specified
  if (!is.null(df[[deparse(substitute(grouping_var))]])) {
    # Initialise a results list
    results <- list()

    # Iterate over each unique group in the grouping variables
    for (group in unique(df[[deparse(substitute(grouping_var))]])) {
      # Create group-specific df
      group_df <- df[df[[deparse(substitute(grouping_var))]] == group, ]

      # Calculate percentiles for group
      if (nrow(group_df) > min_group_size) {
        if (ci) {
          results[[group]] <- calculate_percentiles_ci(df = group_df,
                                                       es = deparse(substitute(es)),
                                                       se = deparse(substitute(se)),
                                                       probs = probs,
                                                       weighted = weighted,
                                                       n_bootstrap = n_bootstrap)
        } else {
          results[[group]] <- calculate_percentiles(df = group_df,
                                                    es = deparse(substitute(es)),
                                                    se = deparse(substitute(se)),
                                                    probs = probs,
                                                    weighted = weighted)
        }
      }
    }

    # Calculate overall percentiles
    if (ci) {
      results[["All"]] <- calculate_percentiles_ci(df = df,
                                                   es = deparse(substitute(es)),
                                                   se = deparse(substitute(se)),
                                                   probs = probs,
                                                   weighted = weighted,
                                                   n_bootstrap = n_bootstrap)
      # Combine results in a dataframe
      results <- bind_rows(results, .id = "Group")

      results <- as.data.frame(results)

    } else {
      results[["All"]] <- calculate_percentiles(df = df,
                                                es = deparse(substitute(es)),
                                                se = deparse(substitute(se)),
                                                probs = probs,
                                                weighted = weighted)

      # Combine results in a dataframe
      results <- bind_rows(results, .id = "Group")
    }

  } else {
    if (ci) {

      results <- calculate_percentiles_ci(df = df,
                                          es = deparse(substitute(es)),
                                          se = deparse(substitute(se)),
                                          probs = probs,
                                          weighted = weighted,
                                          n_bootstrap = n_bootstrap)

    } else {
      results <- calculate_percentiles(df = df,
                                       es = deparse(substitute(es)),
                                       se = deparse(substitute(se)),
                                       probs = probs,
                                       weighted = weighted)
    }

  }

  if (csv_write) {
    write.csv(as.data.frame(results), file = path_file_name)
  }

  return(results)
}
