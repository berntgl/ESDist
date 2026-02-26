#' Title
#'
#' @param df A dataframe.
#' @param es The effect size column.
#' @param se The standard error column (defaults to NULL).
#' @param lim_obj A 'limitmeta' object.
#' @param es_type A string specifying the type of effect size (for x-axis
#' label).
#' @param weighted Defaults to FALSE. If set to TRUE, will weight all effect
#' sizes by their standard error (requires `se` to be defined).
#' @param method Defaults to NULL. Can be set to "quads" (.25, .5, .75) or
#' "thirds" (.1665, .5, .8335) to plot vertical lines based on the respective
#'  benchmarks.
#' @param ci Defaults to FALSE. If set to TRUE, will plot 95% CIs for each
#' benchmark (requires `method` to be defined).
#' @param sesoi Defaults to NULL. When given a value, will calculate the
#' percentage of (weighted) effect sizes larger (or equal to) this value.
#' @param sum_es Defaults to TRUE. Plots the summary effect
#'   size and 95% CI for both the distribution of raw effect sizes as well as
#'   the distribution of adjusted effect sizes.
#' @param sum_es_type Defaults to "diamond". Sets the type of summary effect
#'    size visualisation. "diamond" will create a diamond shape centred around
#'    the summary effect size, with the width of the diamond showing the 95% CI.
#'    Can also be set to "dot", which shows a dot centred on the summary effect
#'    size, with lines and brackets showing the width of the 95% CI.
#' @param bin_width Width of the histogram bins. Defaults to 0.1.
#' @param n_bootstrap Number of bootstrapped samples for benchmark 95% CIs.
#'
#' @returns A 'ggplot' object.
#' @export
#'
#' @examples esd_plot_pba(ot_dat, yi, sei)
esd_plot_pba <- function(df = NULL,
                         es = NULL,
                         se = NULL,
                         lim_obj = NULL,
                         es_type = "Effect size",
                         weighted = FALSE,
                         sesoi = NULL,
                         sum_es = TRUE,
                         method = NULL,
                         abs = FALSE,
                         ci = FALSE,
                         sum_es_type = "diamond",
                         bin_width = 0.1,
                         n_bootstrap = 1000) {

  if (!is.null(df)) {
    df = as.data.frame(df)
    meta_obj <- metagen(TE = df[, deparse(substitute(es))], seTE = df[, deparse(substitute(se))])
    lim_obj <- limitmeta(meta_obj)
  }

  stopifnot(class(lim_obj) == "limitmeta")
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

  if (!missing(sesoi) | !missing(method)) {
    sum_es <- FALSE
  }

  plot <- iceberg_plot(df = df,
                       weighted = weighted,
                       es_type = es_type,
                       sesoi = sesoi,
                       bin_width = bin_width,
                       abs = abs)

  if (!missing(method)) {
    # Random
    plot <- add_benchmarks(plot = plot,
                           df = df,
                           es = "TE",
                           se = "seTE",
                           weighted = weighted,
                           method = method,
                           abs = abs,
                           ci = ci,
                           n_bootstrap = n_bootstrap)

    # Adjusted
    suppressMessages({
      plot <- add_benchmarks(plot = plot,
                             df = df,
                             es = "TE.limit",
                             se = "seTE.limit",
                             weighted = weighted,
                             method = method,
                             abs = abs,
                             ci = ci,
                             n_bootstrap = n_bootstrap,
                             mirrored = TRUE)
    })
  }

  if (sum_es) {
    primary_darkest <- "#00161E"
    if (sum_es_type == "diamond") {
      prange <- max(ggplot_build(plot)$layout$panel_params[[1]]$y$continuous_range)
      diamond <- data.frame(
        x.random = c(df$lower.random[1], df$TE.random[1], df$upper.random[1], df$TE.random[1]),
        y.random = c(0.2*prange, 0.25*prange, 0.2*prange, 0.15*prange),
        x.adjust = c(df$lower.adjust[1], df$TE.adjust[1], df$upper.adjust[1], df$TE.adjust[1]),
        y.adjust = c(-0.2*prange, -0.25*prange, -0.2*prange, -0.15*prange)
      )

      plot <- plot+
        geom_polygon(data = diamond, aes(x = x.random, y = y.random), fill = primary_darkest) +
        geom_polygon(data = diamond, aes(x = x.adjust, y = y.adjust), fill = primary_darkest)


    } else if (sum_es_type == "dot") {
      plot <- plot +
        geom_segment(aes(x = lower.random[1], xend = upper.random[1], y = 2, yend = 2), color = primary_black, linetype = "solid", size = 2)+
        geom_point(aes(x = TE.random[1], y = 2), size = 5, color = primary_black)+
        geom_point(aes(x = lower.random[1], y = 2), pch = "|", size = 5, color = primary_black)+
        geom_point(aes(x = upper.random[1], y = 2), pch = "|", size = 5, color = primary_black)+
        geom_segment(aes(x = lower.adjust[1], xend = upper.adjust[1], y = -2, yend = -2), color = primary_black, linetype = "solid", size = 2)+
        geom_point(aes(x = TE.adjust[1], y = -2), size = 5, color = primary_black)+
        geom_point(aes(x = lower.adjust[1], y = -2), pch = "|", size = 5, color = primary_black)+
        geom_point(aes(x = upper.adjust[1], y = -2), pch = "|", size = 5, color = primary_black)
    } else {
      return(warning("Please enter a valid summary effect size visualisation option "))
    }
  }

  return(plot)
}
