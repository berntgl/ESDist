#' Creating a plot for an effect size distribution
#'
#' @param df Dataset
#' @param es Column name of effect sizes
#' @param es_type A string describing the type of effect size used (e.g., "Cohen's d")
#' @param method Defaults to 'quads', can also be 'thirds'
#' @param bin_width Sets the bin width for the histogram
#'
#' @return A ggplot object
#' @export
#'
#' @examples esd_plot(df, es, es_type = "Cohen's d", method = thirds)
esd_plot <- function(df, es, es_type, method = "quads", bin_width = 0.1) {
  es_col <- df[, deparse(substitute(es))]
  if (method == "quads") {
    q1 <- quantile(es_col, prob = 0.25)
    q1_label <- "25th"
    q2 <- quantile(es_col, prob = 0.50)
    q2_label <- "50th"
    q3 <- quantile(es_col, prob = 0.75)
    q3_label <- "75th"
  } else if (method == "thirds") {
    q1 <- quantile(es_col, prob = 0.1665)
    q1_label <- "16.65th"
    q2 <- quantile(es_col, prob = 0.50)
    q2_label <- "50th"
    q3 <- quantile(es_col, prob = 0.8335)
    q3_label <- "83.35th"
  } else {
    return("Please enter a valid method")
  }
  plot <- ggplot(data = df, aes(es_col)) +
    theme_minimal()+
    geom_histogram(fill = "#355C7D", colour = NA, binwidth = bin_width) +
    scale_x_continuous(breaks = seq(0, 3, 0.5)) +
    geom_vline(aes(xintercept = q1, color = "q1"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = q2, color = "q2"), linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = q3, color = "q3"), linetype = "dashed", size = 1) +
    scale_color_manual(name = "Percentiles",
                       values = c(q1 = "#F8B195", q2 = "#F67280", q3 = "#C06C84"),
                       labels = c(q1_label, q2_label, q3_label))+
    theme(legend.position = c(0.9, 0.7), legend.background = element_rect(fill="#dde7f0", color = "#dde7f0")) +
    labs(x = es_type, y = "Frequency")
  return(plot)
}
