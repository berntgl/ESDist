#' Create a publication bias-adjusted table of effect size benchmarks.
#'
#' @param df A dataframe.
#' @param es The effect size column.
#' @param se The standard error column (defaults to NULL).
#' @param lim_obj A 'limitmeta' object.
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
#' @examples esd_table_pba(ot_dat, yi, sei)
#' @examples esd_table_pba(ot_dat, yi, sei, grouping_var = group)
esd_table_pba <- function(df = NULL,
                          es = NULL,
                          se = NULL,
                          lim_obj = NULL,
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

  # Create limitmeta object with df, es, and se columns.
  if (!missing(df) & missing(grouping_var)) {
    df <- as.data.frame(df)
    meta_obj <- metagen(TE = df[, deparse(substitute(es))],
                        seTE = df[, deparse(substitute(se))])
    lim_obj <- limitmeta(meta_obj)
  } else if (!missing(df) & length(df[, deparse(substitute(grouping_var))]) > 1) {

    # Calculate publication bias-adjusted estimates per group given grouping_var
    df <- as.data.frame(df)
    meta_obj <- metagen(TE = df[, deparse(substitute(es))],
                        seTE = df[, deparse(substitute(se))],
                        subgroup = df[, deparse(substitute(grouping_var))])
    lim_obj <- limitmeta(meta_obj)
  }

  # Extract relevant columns
  stopifnot(class(lim_obj) == "limitmeta")
  if (!missing(grouping_var)) {
    df <- data.frame(lim_obj[1], # TE
                     lim_obj[2], # seTE
                     lim_obj[3], # TE.limit
                     lim_obj[4], # seTE.limit
                     lim_obj[6], # TE.random
                     lim_obj[8], # lower.random
                     lim_obj[9], # upper.random
                     lim_obj[13], # TE.adjust
                     lim_obj[15], # lower.adjust
                     lim_obj[16], # upper.adjust
                     lim_obj[["x"]][["byvar"]])
    df$TE_abs <- abs(df$TE)
    df$TE.limit_abs <- abs(df$TE.limit)
    colnames(df)[11] <- "byvar"
    df <- as.data.frame(df)
  } else {
    df <- data.frame(lim_obj[1], # TE
                     lim_obj[2], # seTE
                     lim_obj[3], # TE.limit
                     lim_obj[4], # seTE.limit
                     lim_obj[6], # TE.random
                     lim_obj[8], # lower.random
                     lim_obj[9], # upper.random
                     lim_obj[13], # TE.adjust
                     lim_obj[15], # lower.adjust
                     lim_obj[16]) # upper.adjust
    df$TE_abs <- abs(df$TE)
    df$TE.limit_abs <- abs(df$TE.limit)
    df <- as.data.frame(df)
  }

  results <- list()

  # Use esd_table function to calculate estimates for original and adjusted effect sizes
  results[["Original"]] <- esd_table(df, TE, seTE,
                                     weighted = weighted,
                                     grouping_var = byvar,
                                     min_group_size = min_group_size,
                                     method = method,
                                     ci = ci,
                                     n_bootstrap = n_bootstrap,
                                     bowley = bowley,
                                     ndec = ndec)


  results[["Adjusted"]] <- esd_table(df, TE.limit, seTE.limit,
                                     weighted = weighted,
                                     grouping_var = byvar,
                                     min_group_size = min_group_size,
                                     method = method,
                                     ci = ci,
                                     n_bootstrap = n_bootstrap,
                                     bowley = bowley,
                                     ndec = ndec)

  results <- bind_rows(results, .id = "Estimate")

  results <- as.data.frame(results)

  if (!missing(grouping_var)) {
    group_levels <- unique(results[["Group"]])

    results <- results |>
      mutate(
        Group = factor(Group, levels = group_levels),
        Estimate = factor(Estimate, levels = c("Original", "Adjusted"))
      ) |>
      arrange(Group, Estimate) |>
      relocate(Group)
  }


  if (csv_write == TRUE) {
    write.csv(adj_values, file = path_file_name)
  }
  return(results)
}
