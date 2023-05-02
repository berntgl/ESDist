#' Creating a plot for an effect size distribution
#'
#' @param df Dataset
#' @param es Column name of effect sizes
#' @param es_type A string describing the type of effect size used (e.g.,
#'   "Cohen's d")
#' @param method Defaults to FALSE, can also be 'thirds' for 16.65th, 50th, and
#'   83.35th percentiles, or 'quads' for 25th, 50th, and 75th percentiles.
#' @param mean Defaults to NULL, but will insert a ggplot geom_vline element
#'   that corresponds to the mean effect size if set to 'mean' or takes on
#'   a numeric argument to generate a geom_vline element that corresponds to
#'   the inputted value.
#' @param sesoi A numeric argument that corresponds to the population ES of
#'   interest. This will split the histogram into two parts around the inputted
#'   value.
#' @param bin_width Numeric argument that corresponds to the bin width for the
#'   histogram. Defaults to 0.1
#'
#' @return A ggplot object
#' @export
#'
#' @examples esd_plot(ot_dat, yi, es_type = "Cohen's d", method = "thirds")
esd_plot <- function(df,
                     es,
                     es_type,
                     method = FALSE,
                     mean = NULL,
                     sesoi = NULL,
                     bin_width = 0.1) {
  df <- as.data.frame(df)
  es_col <- df[, deparse(substitute(es))]

  if (missing(sesoi)){
    plot <- ggplot(data = df, aes(es_col)) +
      geom_histogram(fill = "#355C7D", binwidth = bin_width) +
      #scale_x_continuous(breaks = seq(0, 3, 0.5)) +
      labs(x = es_type, y = "Frequency")+
      theme_minimal() +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=20))
  } else {
    rank <- length(es_col[es_col < sesoi])/length(es_col) * 100
    rank_rev <- 100 - rank

    rank_perc <- sprintf("%.2f%%", rank)
    rank_rev_perc <- sprintf("%.2f%%", rank_rev)

    plot <- ggplot(data = df) +
      geom_histogram(aes(es_col, fill = after_stat(x) > sesoi),
                     binwidth = bin_width) +
      scale_fill_manual(name = sprintf("ES < or > %.2f", sesoi),
                        labels = c(rank_perc, rank_rev_perc),
                        values = c("#EEE0CB", "#355C7D")) +
      labs(x = es_type, y = "Frequency")+
      theme_minimal()+
      theme(legend.position = c(0.9, 0.7),
            legend.background = element_rect(fill="#dde7f0", color = "#dde7f0"),
            legend.title = element_text(size=14),
            legend.text = element_text(size=12),
            legend.key.size = unit(1, 'cm'),
            axis.text = element_text(size=12),
            axis.title = element_text(size=20))
  }

  if (!isFALSE(method)) {
    if (method == "quads") {
    q1 <- quantile(es_col, prob = 0.25)
    q1_label <- "25th"
    q2 <- quantile(es_col, prob = 0.50)
    q2_label <- "50th"
    q3 <- quantile(es_col, prob = 0.75)
    q3_label <- "75th"

    plot <- plot +
      geom_vline(aes(xintercept = q1, color = "q1"),
                 linetype = "dashed",
                 size = 1) +
      geom_vline(aes(xintercept = q2,
                     color = "q2"),
                 linetype = "dashed",
                 size = 1) +
      geom_vline(aes(xintercept = q3, color = "q3"),
                 linetype = "dashed",
                 size = 1) +
      scale_color_manual(name = "Percentiles",
                         values = c(q1 = "#F8B195",
                                    q2 = "#F67280",
                                    q3 = "#C06C84"),
                         labels = c(q1 = q1_label,
                                    q2 = q2_label,
                                    q3 = q3_label))+
      theme(legend.position = c(0.9, 0.7),
            legend.background = element_rect(fill="#dde7f0",
                                             color = "#dde7f0"))

    } else if (method == "thirds") {
      q1 <- quantile(es_col, prob = 0.1665)
      q1_label <- "16.65th"
      q2 <- quantile(es_col, prob = 0.50)
      q2_label <- "50th"
      q3 <- quantile(es_col, prob = 0.8335)
      q3_label <- "83.35th"
      plot <- plot +
        geom_vline(aes(xintercept = q1, color = "q1"),
                   linetype = "dashed",
                   size = 1) +
        geom_vline(aes(xintercept = q2, color = "q2"),
                   linetype = "dashed",
                   size = 1) +
        geom_vline(aes(xintercept = q3, color = "q3"),
                   linetype = "dashed",
                   size = 1) +
        scale_color_manual(name = "Percentiles",
                           values = c(q1 = "#F8B195",
                                      q2 = "#F67280",
                                      q3 = "#C06C84"),
                           labels = c(q1 = q1_label,
                                      q2 = q2_label,
                                      q3 = q3_label))+
        theme(legend.position = c(0.9, 0.7),
              legend.background = element_rect(fill="#dde7f0",
                                               color = "#dde7f0"))
    } else {
      return("Please enter a valid method")
    }
  } else if (isFALSE(method)) {
    plot <- plot
  } else {
    return("Please enter a valid method")
  }
  if (!missing(mean)) {
    if (mean == "mean") {
      plot <- plot +
        geom_vline(aes(xintercept = mean(es_col)), color = "#7DAA92",
                   linetype = "dotted", size = 1)
    } else if (is.numeric(mean)) {
      plot <- plot +
        geom_vline(aes(xintercept = mean), color = "#7DAA92",
                   linetype = "dotted", size = 1)
    } else {
      return("Please enter a valid mean value")
    }
  } else {
    plot <- plot
  }
  return(plot)
}
