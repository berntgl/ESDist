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
#' @param pop_es A numeric argument that corresponds to the population ES of
#'   interest. This will split the histogram into two parts around the inputted
#'   value.
#' @param bin_width Numeric argument that corresponds to the bin width for the
#'   histogram. Defaults to 0.1
#'
#' @return A ggplot element
#' @export
#'
#' @examples
esd_plot_group <- function(df, es, es_type, grouping_var, method = FALSE, mean = FALSE, pop_es = NULL, bin_width = 0.1) {
  es_col <- df[, deparse(substitute(es))]

  dat_b <- df %>%
    group_by({{grouping_var}}) %>%
    mutate(mean = mean({{es}}),
           q16 = quantile({{es}}, prob = 0.1665),
           q25 = quantile({{es}}, prob = 0.25),
           q50 = quantile({{es}}, prob = 0.50),
           q75 = quantile({{es}}, prob = 0.75),
           q83 = quantile({{es}}, prob = 0.8335),) %>%
    ungroup()
  q16_label <- "16.65th"
  q25_label <- "25th"
  q50_label <- "50th"
  q75_label <- "75th"
  q83_label <- "83.35th"

  if (missing(pop_es)){
    plot <- ggplot(data = dat_b)+
      geom_histogram(aes({{es}}), fill = "#355C7D", binwidth = bin_width)+
      scale_x_continuous(breaks = seq(0, 5, 0.5)) +
      labs(x = es_type, y = "Frequency")+
      theme_minimal() +
      facet_grid(vars({{grouping_var}}),
                 switch = "y")+
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=20),
            strip.text.y.left = element_text(angle = 0,
                                             size = 12),
            legend.position = "bottom")
  } else {
    rank <- length(es_col[es_col <= pop_es])/length(es_col) * 100
    rank_rev <- 100 - rank

    rank_perc <- sprintf("%.2f%%", rank)
    rank_rev_perc <- sprintf("%.2f%%", rank_rev)

    plot <- ggplot(data = dat_b) +
      geom_histogram(aes(es_col, fill = stat(x) > pop_es), binwidth = bin_width) +
      scale_fill_manual(name = sprintf("ES < or > %.2f", pop_es),
                        labels = c(rank_perc, rank_rev_perc),
                        values = c("#EEE0CB", "#355C7D")) +
      labs(x = es_type, y = "Frequency")+
      theme_minimal()+
      facet_grid(vars({{grouping_var}}),
                 switch = "y")+
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=20),
            strip.text.y.left = element_text(angle = 0,
                                             size = 12),
            legend.position = "bottom")
  }
  if (!isFALSE(method)) {
    if (method == "quads") {
      plot <- plot +
        geom_vline(aes(xintercept = q25, color = "q25"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = q50, color = "q50"), linetype = "dashed", size = 1) +
        geom_vline(aes(xintercept = q75, color = "q75"), linetype = "dashed", size = 1) +
        scale_color_manual(name = "Percentiles",
                           values = c(q25 = "#F8B195",
                                      q50 = "#F67280",
                                      q75 = "#C06C84",
                                      q4 = "#7DAA92"),
                           labels = c(q25_label, q50_label, q75_label, "Mean"))
      } else if (method == "thirds") {
        plot <- plot+
          geom_vline(aes(xintercept = q16, color = "q16"), linetype = "dashed", size = 1) +
          geom_vline(aes(xintercept = q50, color = "q50"), linetype = "dashed", size = 1) +
          geom_vline(aes(xintercept = q83, color = "q83"), linetype = "dashed", size = 1) +
          scale_color_manual(name = "Percentiles",
                             values = c(q16 = "#F8B195",
                                        q50 = "#F67280",
                                        q83 = "#C06C84",
                                        q4 = "#7DAA92"),
                             labels = c(q16_label, q50_label, q83_label, "Mean"))
      } else {
        return("Please enter a valid method")
      }
  } else if (isFALSE(method)) {
    plot <- plot
  } else {
    return("Please enter a valid method")
  }
  if (mean == TRUE) {
    plot <- plot +
      geom_vline(aes(xintercept = mean, color = "q4"), linetype = "dotted", size = 1)
  } else {
    plot <- plot
  }
 return(plot)
}
