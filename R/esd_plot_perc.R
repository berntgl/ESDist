#' Plot effect sizes as a function of percentile
#'
#' @param df Dataset
#' @param es Column name for effect sizes
#' @param es_type String describing the type of effect size used.
#' @param method Defaults to 'quads', can also be thirds
#'
#' @return ggplot object
#' @export
#'
#' @examples esd_plot_perc(df, es, "Cohen's d")
esd_plot_perc <- function(df, es, es_type, method = 'quads') {
  es_col <- df[, deparse(substitute(es))]
  if (method == "quads") {
    q1 <- quantile(es_col, prob = 0.25)
    q1_label <- "25th"
    q1_int <- .25
    q2 <- quantile(es_col, prob = 0.50)
    q2_label <- "50th"
    q2_int <- .5
    q3 <- quantile(es_col, prob = 0.75)
    q3_label <- "75th"
    q3_int <- .75
  } else if (method == "thirds") {
    q1 <- quantile(es_col, prob = 0.1665)
    q1_label <- "16.65th"
    q1_int <- .1665
    q2 <- quantile(es_col, prob = 0.50)
    q2_label <- "50th"
    q2_int <- .5
    q3 <- quantile(es_col, prob = 0.8335)
    q3_label <- "83.35th"
    q3_int <- .8335
  } else {
    return("Please enter a valid method")
  }

  df <- df %>%
    mutate(rank = percent_rank({{es}}))

  plot <- ggplot(data = df, aes(x = rank, y=es_col, col="#355C7D")) +
    theme_minimal() +
    geom_point(col = "#355C7D") +
    geom_smooth(size = 1, col="#355C7D") +
    geom_vline(aes(xintercept = q1_int, col = "q1_int"), linetype = "dashed", size = 1)+
    geom_vline(aes(xintercept = q2_int, col = "q2_int"),linetype = "dashed", size = 1)+
    geom_vline(aes(xintercept = q3_int, col = "q3_int"), linetype = "dashed", size = 1)+
    scale_color_manual(name = "Percentiles",
                       values = c(q1_int = "#F8B195", q2_int= "#F67280", q3_int = "#C06C84"),
                       labels = c(q1_label, q2_label, q3_label))+
    theme(legend.position = c(0.1, 0.7), legend.background = element_rect(fill="#dde7f0", color = "#dde7f0")) +
    xlab("Percentile")+
    ylab(es_type)
  return(plot)
}
