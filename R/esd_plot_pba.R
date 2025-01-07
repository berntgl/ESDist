#' Creating an iceberg plot to visualise publication bias adjustment.
#'
#' @param df Dataset.
#' @param es Column name of effect sizes.
#' @param se Column name of standard error.
#' @param lim_obj An object of class "limitmeta".
#' @param es_type A string describing the type of effect size used (e.g.,
#'   "Cohen's d").
#' @param weighted Defaults to FALSE. When set to TRUE, will calculate the
#'  weighted distribution based on the inverse standard error.
#' @param sesoi A numeric argument that corresponds to the population ES of
#'   interest. This will split the histogram into two parts around the inputted
#'   value.
#' @param sum_es Defaults to TRUE. Plots the summary effect
#'   size and 95% CI for both the distribution of raw effect sizes as well as
#'   the distribution of adjusted effect sizes.
#' @param method Defaults to FALSE, can also be 'thirds' for 16.65th, 50th, and
#'   83.35th percentiles, or 'quads' for 25th, 50th, and 75th percentiles.
#' @param abs Defaults to FALSE. If set to TRUE, plots all effect sizes as
#'    absolute values.
#' @param sum_es_type Defaults to "diamond". Sets the type of summary effect
#'    size visualisation. "diamond" will create a diamond shape centred around
#'    the summary effect size, with the width of the diamond showing the 95% CI.
#'    Can also be set to "dot", which shows a dot centred on the summary effect
#'    size, with lines and brackets showing the width of the 95% CI.
#' @param bin_width Numeric argument that corresponds to the bin width for the
#'   histogram. Defaults to 0.1
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' esd_plot_pba(df = ot_dat, es = yi, se = sei, es_type = "Cohen's d")
#'
esd_plot_pba <- function(df = NULL,
                         es = NULL,
                         se = NULL,
                         lim_obj = NULL,
                         es_type,
                         weighted = FALSE,
                         sesoi = NULL,
                         sum_es = TRUE,
                         method = FALSE,
                         abs = FALSE,
                         sum_es_type = "diamond",
                         bin_width = 0.1) {

  primary_darkest <- "#00161E"
  primary_dark <- "#07445B"
  primary_medium <- "#206985"
  primary_light <- "#C6DFE8"
  secondary_dark <- "#BBBBBB"
  secondary_light <- "#E7E7E7"
  benchmarks1 <- "#FF8C77"
  benchmarks2 <- "#D5462C"
  benchmarks3 <- "#921B05"
  accent <- "#D5A42C"

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
  df$TE_abs <- abs(df$TE)
  df$TE.limit_abs <- abs(df$TE.limit)
  if (!isFALSE(method)) {
    abs = TRUE
    sum_es = FALSE
  }
  if (!missing(sesoi)) {
    abs = TRUE
    sum_es = FALSE
  }

  if (isTRUE(weighted)) {
    y_label <- "Weighted count"
    df$weights <- 1 / df$seTE
    df$weights.limit = 1 / df$seTE.limit
  } else {
    y_label <- "Count"
    df$weights <- 1
    df$weights.limit <- 1
  }

  if (isFALSE(abs)) {
    plot <- ggplot(data = df) +
      geom_histogram(aes(x = TE, weight = weights), fill=primary_light, binwidth = bin_width) +
      geom_histogram(aes(x = TE.limit, y = -after_stat(count), weight = weights.limit), fill = primary_dark, binwidth = bin_width) +
      theme_minimal() +
      geom_hline(aes(yintercept = 0), size = 0.2)+
      labs(x = es_type, y = y_label)+
      theme(
        axis.text = element_text(size=12),
        axis.title = element_text(size=20))+
      scale_y_continuous(labels = function(x) abs(x))
  } else if (!isFALSE(abs)) {
    if (missing(sesoi)){
      plot <- ggplot(data = df) +
        geom_histogram(aes(x = TE_abs, weight = weights), fill=primary_light, binwidth = bin_width, center = (bin_width / 2)) +
        geom_histogram(aes(x = TE.limit_abs, y = -after_stat(count), weight = weights.limit), fill = primary_dark, binwidth = bin_width, center = (bin_width / 2)) +
        theme_minimal() +
        geom_hline(aes(yintercept = 0), size = 0.2)+
        labs(x = es_type, y = y_label)+
        theme(
          axis.text = element_text(size=12),
          axis.title = element_text(size=20))+
        scale_y_continuous(labels = function(x) abs(x))
    } else {
      rank_random <- ifelse(isTRUE(weighted),
             sum(df$weights[df$TE_abs < sesoi]) / sum(df$weights) * 100,
             length(df$TE_abs[df$TE_abs < sesoi])/length(df$TE_abs) * 100)
      rank_rev_random <- 100 - rank_random

      rank_adjust <- ifelse(isTRUE(weighted),
                            sum(df$weights.limit[df$TE.limit_abs < sesoi]) / sum(df$weights.limit) * 100,
                            length(df$TE.limit_abs[df$TE.limit_abs < sesoi])/length(df$TE.limit_abs) * 100)
      rank_rev_adjust <- 100 - rank_adjust

      rank_perc_random <- sprintf("%.2f%%", rank_random)
      rank_rev_perc_random <- sprintf("%.2f%%", rank_rev_random)

      rank_perc_adjust <- sprintf("%.2f%%", rank_adjust)
      rank_rev_perc_adjust <- sprintf("%.2f%%", rank_rev_adjust)

      plot <- ggplot(data = df) +
        geom_histogram(aes(x = TE_abs, weight = weights, fill = ifelse(after_stat(x) > sesoi, "B", "A")), binwidth = bin_width, center = (bin_width / 2)) +
        geom_histogram(aes(x = TE.limit_abs, y = -after_stat(count), weight = weights.limit, fill = ifelse(after_stat(x) > sesoi, "D", "C")), binwidth = bin_width, center = (bin_width / 2)) +
        theme_minimal() +
        geom_hline(aes(yintercept = 0), size = 0.2)+
        scale_fill_manual(name = sprintf("ES < or > %.2f", sesoi),
                          labels = c("A" = rank_perc_random, "B" = rank_rev_perc_random, "C" = rank_perc_adjust, "D" = rank_rev_perc_adjust),
                          values = c("A" = secondary_light, "B" = primary_light, "C" = secondary_dark, "D" = primary_dark)) +
        labs(x = es_type, y = y_label)+
        theme(legend.position = "bottom",
              #legend.background = element_rect(fill="#dde7f0", color = "#dde7f0"),
              legend.title = element_text(size=14, hjust = 0.5),
              legend.text = element_text(size=12),
              legend.key.size = unit(1, 'cm'),
              axis.text = element_text(size=12),
              axis.title = element_text(size=20))+
        scale_y_continuous(labels = function(x) abs(x))
    }
  } else {
    return(warning("No valid value for 'abs'"))
  }

  if (!isFALSE(sum_es) & isFALSE(method)) {
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
  } else if (!isFALSE(method) & isFALSE(sum_es)) {
    if (method == "quads") {
      annotation <- data.frame(
        x1 = c(wtd.quantile(df$TE_abs, weights = df$weights, prob = 0.25), wtd.quantile(df$TE_abs, weights = df$weights, prob = 0.50), wtd.quantile(df$TE_abs, weights = df$weights, prob = 0.75)),
        x2 = c(wtd.quantile(df$TE.limit_abs, weights = df$weights.limit, prob = 0.25), wtd.quantile(df$TE.limit_abs, weights = df$weights.limit, prob = 0.5), wtd.quantile(df$TE.limit_abs, weights = df$weights.limit, prob = 0.75)),
        label = c("25th", "50th", "75th")
      )
      plot <- plot +
        geom_segment(data=annotation,
                     aes(x = x1, y=0, yend=Inf, color = label),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(data=annotation,
                     aes(x = x2, y=0, yend=-Inf, color = label),
                     linetype = "dashed",
                     size = 1) +
        scale_color_manual(name = "Percentiles",
                           values = c("25th" = benchmarks1,
                                      "50th" = benchmarks2,
                                      "75th" = benchmarks3))+
        theme(legend.position = "bottom",
              panel.spacing = unit(2, "lines"),
              legend.title = element_text(size=14),
              legend.text = element_text(size=12),
              legend.key.size = unit(1, 'cm'))

    } else if (method == "thirds") {
      annotation <- data.frame(
        x1 = c(wtd.quantile(df$TE_abs, weights = df$weights, prob = 0.1665), wtd.quantile(df$TE_abs, weights = df$weights, prob = 0.50), wtd.quantile(df$TE_abs, weights = df$weights, prob = 0.8335)),
        x2 = c(wtd.quantile(df$TE.limit_abs, weights = df$weights.limit, prob = 0.1665), wtd.quantile(df$TE.limit_abs, weights = df$weights.limit, prob = 0.5), wtd.quantile(df$TE.limit_abs, weights = df$weights.limit, prob = 0.8335)),
        label = c("16.65th", "50th", "83.35th")
      )

      plot <- plot +
        geom_segment(data=annotation,
                     aes(x = x1, y=0, yend=Inf, color = label),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(data=annotation,
                     aes(x = x2, y=0, yend=-Inf, color = label),
                     linetype = "dashed",
                     size = 1) +
        scale_color_manual(name = "Percentiles",
                           values = c("16.65th" = benchmarks1,
                                      "50th" = benchmarks2,
                                      "83.35th" = benchmarks3))+
        theme(legend.position = "bottom",
              panel.spacing = unit(2, "lines"),
              legend.title = element_text(size=14),
              legend.text = element_text(size=12),
              legend.key.size = unit(1, 'cm'))

      } else {
        return(warning("Please enter a valid method"))
      }
    } else if (isFALSE(method) & isFALSE(sum_es)) {
      plot <- plot
    } else {
      return(warning("Cannot plot summary effect size and benchmarks simultaneously"))
    }
  return(plot)
}
