#' Creating an ESD plot per group
#'
#' @param df Dataset
#' @param es Column name of effect sizes
#' @param es_type A string describing the type of effect size used (e.g.,
#'   "Cohen's d")
#' @param grouping_var Column name of grouping variable
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
  benchmarks1 <- "#FFADA0"
  benchmarks2 <- "#D5462C"
  benchmarks3 <- "#110301"
  accent <- "#FFCF57"

  df <- as.data.frame(df)
  df$es_abs <- abs(df[, deparse(substitute(es))])

  if (!isFALSE(method)) {
    abs = TRUE
  }

  dat_b <- df %>%
    group_by({{grouping_var}}) %>%
    mutate(mean = mean(es_abs),
           q16 = quantile(es_abs, prob = 0.1665),
           q25 = quantile(es_abs, prob = 0.25),
           q50 = quantile(es_abs, prob = 0.50),
           q75 = quantile(es_abs, prob = 0.75),
           q83 = quantile(es_abs, prob = 0.8335),
           count = n()) %>%
    filter(count >= min_group_size) %>%
    ungroup()
  q16_label <- "16.65th"
  q25_label <- "25th"
  q50_label <- "50th"
  q75_label <- "75th"
  q83_label <- "83.35th"


  if(isFALSE(abs)) {
    plot <- ggplot(data = dat_b)+
      geom_histogram(aes({{es}}), fill = primary_dark, binwidth = bin_width)+
      labs(x = es_type, y = "Frequency")+
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
    plot <- ggplot(data = dat_b)+
      geom_histogram(aes(es_abs), fill = primary_dark, binwidth = bin_width)+
      labs(x = es_type, y = "Frequency")+
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
      plot <- plot +ß
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


