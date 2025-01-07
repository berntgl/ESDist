#' Creating an ESD plot per group
#'
#' @param df Dataset
#' @param es Column name of effect sizes
#' @param es_type A string describing the type of effect size used (e.g.,
#'   "Cohen's d")
#' @param grouping_var Column name of grouping variable
#' @param se Column name of standard error.
#' @param weighted Defaults to FALSE. When set to TRUE, will calculate the
#'  weighted distribution based on the inverse standard error.
#' @param method Defaults to FALSE, but can be 'quads' or 'thirds'
#' @param mean Defaults to FALSE, but will insert a ggplot geom_vline element
#'   that corresponds to the mean effect size
#' @param abs Defaults to FALSE. If set to TRUE, plots all effect sizes as
#'    absolute values
#' @param min_group_size Numeric value corresponding to the minimum amount of
#' effect sizes for a group to be included in the plot. Defaults to 20.
#' @param bin_width Numeric argument that corresponds to the bin width for the
#'   histogram. Defaults to 0.1
#'
#' @return A ggplot element
#' @export
#'
#' @examples esd_plot_group(df = ot_dat,
#'                          es = yi,
#'                          grouping_var = group,
#'                          es_type = "Hedges' g")

esd_plot_group <- function(df,
                           es,
                           es_type,
                           grouping_var,
                           se = NULL,
                           weighted = FALSE,
                           method = FALSE,
                           mean = FALSE,
                           abs = FALSE,
                           min_group_size = 20,
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

  df <- as.data.frame(df)
  df$es_col <- df[, deparse(substitute(es))]
  df$es_col_abs <- abs(df[, deparse(substitute(es))])

  if (!isFALSE(method)) {
    abs = TRUE
  }

  if (isTRUE(weighted)) {
    stopifnot(!missing(se))
    y_label <- "Weighted count"
    df$se <- df[, deparse(substitute(se))]
    df$weights <- 1 / df$se
  } else {
    y_label <- "Count"
    df$weights <- 1
  }

  df <- df %>%
    group_by({{grouping_var}}) %>%
    mutate(q16 = wtd.quantile(es_col_abs, weights = weights, prob = 0.1665),
           q25 = wtd.quantile(es_col_abs, weights = weights, prob = 0.25),
           q50 = wtd.quantile(es_col_abs, weights = weights, prob = 0.5),
           q75 = wtd.quantile(es_col_abs, weights = weights, prob = 0.75),
           q83 = wtd.quantile(es_col_abs, weights = weights, prob = 0.8335),
           count = n()) %>%
    filter(count >= min_group_size) %>%
    ungroup()
  q16_label <- "16.65th"
  q25_label <- "25th"
  q50_label <- "50th"
  q75_label <- "75th"
  q83_label <- "83.35th"

  if(isFALSE(abs)) {
    plot <- ggplot(data = df)+
      geom_histogram(aes(es_col, weight = weights), fill = primary_dark, binwidth = bin_width)+
      labs(x = es_type, y = y_label)+
      theme_minimal() +
      facet_grid(vars({{grouping_var}}),
                 switch = "y")+
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=16),
            strip.text.y.left = element_text(angle = 0,
                                             size = 12),
            legend.position = "bottom",
            panel.spacing = unit(2, "lines"))
  } else if (!isFALSE(abs)) {
    plot <- ggplot(data = df)+
      geom_histogram(aes(es_col_abs, weight = weights), fill = primary_dark, binwidth = bin_width, center = (bin_width / 2))+
      labs(x = es_type, y = y_label)+
      theme_minimal() +
      facet_grid(vars({{grouping_var}}),
                 switch = "y")+
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=16),
            strip.text.y.left = element_text(angle = 0,
                                             size = 12),
            legend.position = "bottom",
            panel.spacing = unit(2, "lines"))
    if (!isFALSE(method)) {
      if (method == "quads") {

        plot <- plot +
          geom_vline(aes(xintercept = q25, color = "q25"),
                     linetype = "dashed",
                     size = 1) +
          geom_vline(aes(xintercept = q50, color = "q50"),
                     linetype = "dashed",
                     size = 1) +
          geom_vline(aes(xintercept = q75, color = "q75"),
                     linetype = "dashed",
                     size = 1) +
          scale_color_manual(name = "Percentiles",
                             values = c(q25 = benchmarks1,
                                        q50 = benchmarks2,
                                        q75 = benchmarks3),
                             labels = c(q25 = q25_label,
                                        q50 = q50_label,
                                        q75 = q75_label))
      } else if (method == "thirds") {

        plot <- plot+
          geom_vline(aes(xintercept = q16, color = "q16"),
                     linetype = "dashed",
                     size = 1) +
          geom_vline(aes(xintercept = q50, color = "q50"),
                     linetype = "dashed",
                     size = 1) +
          geom_vline(aes(xintercept = q83, color = "q83"),
                     linetype = "dashed",
                     size = 1) +
          scale_color_manual(name = "Percentiles",
                             values = c(q16 = benchmarks1,
                                        q50 = benchmarks2,
                                        q83 = benchmarks3),
                             labels = c(q16 = q16_label,
                                        q50 = q50_label,
                                        q83 = q83_label))
      } else {
        return(warning("Please enter a valid method"))
      }
    } else if (isFALSE(method)) {
      plot <- plot
    } else {
      return(warning("Please enter a valid method"))
    }
  } else {
    return(warning("No valid value for 'abs'"))
  }

  if (!missing(mean)) {
    if (mean == "mean") {
      plot <- plot +
        geom_vline(aes(xintercept = mean), color = accent,
                   linetype = "dotted", size = 1)
    } else if (is.numeric(mean)) {
      plot <- plot +
        geom_vline(aes(xintercept = mean), color = accent,
                   linetype = "dotted", size = 1)
    } else {
      return(warning("Please enter a valid mean value"))
    }
  } else {
    plot <- plot
  }
 return(plot)
}


