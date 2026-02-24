#' Create a table of effect size benchmarks.
#'
#' @param df A dataframe.
#' @param es The effect size column.
#' @param se The standard error column (defaults to NULL).
#' @param grouping_var Column name of grouping variable.
#' @param min_group_size Sets the minimum amount of effect sizes in each group
#' as specified by the `grouping_var` variable. Defaults to 20.
#' @param weighted Defaults to FALSE. If set to TRUE, will weight all effect
#' sizes by their standard error (requires `se` to be defined).
#' @param method Defaults to "quads". Can be set to "quads" (.25, .5, .75) or
#' "thirds" (.1665, .5, .8335) to plot vertical lines based on the respective
#'  benchmarks.
#' @param ci Defaults to FALSE. If set to TRUE, will plot 95% CIs for each
#' benchmark (requires `method` to be defined).
#' @param n_bootstrap Number of bootstrapped samples for benchmark 95% CIs.
#' @param bowley Defaults to FALSE. Will calculate Bowley's skewness when set
#' to TRUE.
#' @param csv_write Defaults to FALSE. Will write the outputted table as a csv.
#' @param path_file_name A string containing the directory to which the .csv
#' file will be saved, including the title of the .csv file (has to end in
#' '.csv').
#' @param ndec Number of decimals. Defaults to 2.
#'
#' @returns A table of class 'data.frame'.
#' @export
#'
#' @examples esd_table(ot_dat, yi)
#' @examples esd_table(ot_dat, yi, grouping_var = group)
esd_table <- function(df,
                      es,
                      se = NULL,
                      grouping_var = NULL,
                      min_group_size = 3,
                      weighted = FALSE,
                      method = "quads",
                      ci = FALSE,
                      n_bootstrap = 1000,
                      bowley = FALSE,
                      csv_write = FALSE,
                      path_file_name = "esd_table.csv",
                      ndec = 2) {

  # Define the probs based on the specified method
  if (method == "quads") {
    probs <- c(0.25, 0.5, 0.75)
  } else if (method == "thirds") {
    probs <- c(0.1665, 0.5, 0.8335)
    if (bowley) {
      bowley <- FALSE
      warning("Can't calculate Bowley's coefficient for method 'thirds'.")
    }
  } else {
    stop(paste("No valid method specified: ", method, ". \n Method has to be 'quads' or 'thirds'."))
  }


  # Condition on a grouping var being specified
  if (!is.null(df[[deparse(substitute(grouping_var))]])) {
    # Initialise a results list
    results <- list()

    # Arrange groups in alphabetical order
    unique_groups <- unique(df[,deparse(substitute(grouping_var))])
    unique_groups <- sort(unique_groups)

    # Iterate over each unique group in the grouping variables
    for (group in unique_groups) {
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
                                                       bowley = bowley,
                                                       n_bootstrap = n_bootstrap,
                                                       ndec = ndec)
        } else {
          results[[group]] <- calculate_percentiles(df = group_df,
                                                    es = deparse(substitute(es)),
                                                    se = deparse(substitute(se)),
                                                    probs = probs,
                                                    weighted = weighted,
                                                    bowley = bowley,
                                                    ndec = ndec)
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
                                                   bowley = bowley,
                                                   n_bootstrap = n_bootstrap,
                                                   ndec = ndec)
      # Combine results in a dataframe
      results <- bind_rows(results, .id = "Group")

    } else {
      results[["All"]] <- calculate_percentiles(df = df,
                                                es = deparse(substitute(es)),
                                                se = deparse(substitute(se)),
                                                probs = probs,
                                                weighted = weighted,
                                                bowley = bowley,
                                                ndec = ndec)

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
                                          bowley = bowley,
                                          n_bootstrap = n_bootstrap,
                                          ndec = ndec)



    } else {
      results <- calculate_percentiles(df = df,
                                       es = deparse(substitute(es)),
                                       se = deparse(substitute(se)),
                                       probs = probs,
                                       weighted = weighted,
                                       bowley = bowley,
                                       ndec = ndec)
    }

  }

  if (csv_write) {
    write.csv(as.data.frame(results), file = path_file_name)
  }

  return(results)
}
