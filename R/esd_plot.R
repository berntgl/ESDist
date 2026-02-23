#' Plotting effect size distributions.
#'
#' @param df A dataframe.
#' @param es The effect size column.
#' @param se The standard error column (defaults to NULL).
#' @param es_type A string specifying the type of effect size (for x-axis
#' label).
#' @param grouping_var Column name of grouping variable.
#' @param min_group_size Sets the minimum amount of effect sizes in each group
#' as specified by the `grouping_var` variable. Defaults to 20.
#' @param weighted Defaults to FALSE. If set to TRUE, will weight all effect
#' sizes by their standard error (requires `se` to be defined).
#' @param method Defaults to NULL. Can be set to "quads" (.25, .5, .75) or
#' "thirds" (.1665, .5, .8335) to plot vertical lines based on the respective
#'  benchmarks.
#' @param ci Defaults to FALSE. If set to TRUE, will plot 95% CIs for each
#' benchmark (requires `method` to be defined).
#' @param sesoi Defaults to NULL. When given a value, will calculate the
#' percentage of (weighted) effect sizes larger (or equal to) this value.
#' @param bin_width Width of the histogram bins. Defaults to 0.1.
#' @param n_bootstrap Number of bootstrapped samples for benchmark 95% CIs.
#'
#' @returns A 'ggplot' object.
#' @export
#'
#' @examples esd_plot(ot_dat, yi)
#' @examples esd_plot(ot_dat, yi, grouping_var = group)

esd_plot <- function(df,
                     es,
                     se = NULL,
                     es_type = "Effect size",
                     grouping_var = NULL,
                     min_group_size = 20,
                     weighted = FALSE,
                     method = NULL,
                     ci = FALSE,
                     sesoi = NULL,
                     bin_width = 0.1,
                     n_bootstrap = 1000) {

  # Convert column arguments to strings
  es <- deparse(substitute(es))

  if (!missing(se)) {
    se <- deparse(substitute(se))
  }

  if (!missing(grouping_var)) {
    grouping_var <- deparse(substitute(grouping_var))
  }

  # Create basic plot
  plot <- basic_plot(df = df,
                     es = es,
                     se = se,
                     grouping_var = grouping_var,
                     min_group_size = min_group_size,
                     weighted = weighted,
                     sesoi = sesoi,
                     bin_width = bin_width)


  # Add benchmarks if specified
  if(!missing(method)) {
    plot <- add_benchmarks(plot = plot,
                           df = df,
                           es = es,
                           se = se,
                           grouping_var = grouping_var,
                           min_group_size = min_group_size,
                           weighted = weighted,
                           method = method,
                           ci = ci,
                           n_bootstrap = n_bootstrap)
  }

  return(plot)
}
