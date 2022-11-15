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
    q1_label <- "25%"
    q1_int <- .25
    q2 <- quantile(es_col, prob = 0.50)
    q2_label <- "50%"
    q2_int <- .5
    q3 <- quantile(es_col, prob = 0.75)
    q3_label <- "75%"
    q3_int <- .75
  } else if (method == "thirds") {
    q1 <- quantile(es_col, prob = 0.1665)
    q1_label <- "16.65%"
    q1_int <- .1665
    q2 <- quantile(es_col, prob = 0.50)
    q2_label <- "50%"
    q2_int <- .5
    q3 <- quantile(es_col, prob = 0.8335)
    q3_label <- "83.35%"
    q3_int <- .8335
  } else {
    return("Please enter a valid method")
  }

  df <- df %>%
    mutate(rank = percent_rank({{es}}))

  plot <- ggplot(data = df, aes(x = rank, y=es_col, col="#27708C")) +
    geom_point(col = "#27708C") +
    geom_smooth(size = 1, col="#27708C") +
    geom_vline(xintercept = q1_int, linetype = "dashed", col = "#85A693", size = 1)+
    geom_vline(xintercept = q2_int, linetype = "dashed", col = "#BFA575", size = 1)+
    geom_vline(xintercept = q3_int, linetype = "dashed", col = "#A6511F", size = 1)+
    xlab("Percentile")+
    ylab(es_type)
  return(plot)
}
