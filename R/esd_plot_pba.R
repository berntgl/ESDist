#' Creating an iceberg plot to visualise publication bias adjustment.
#'
#' @param lim_obj An object of class "limitmeta".
#' @param es_type A string describing the type of effect size used (e.g.,
#'   "Cohen's d")
#' @param sum_es Defaults to TRUE. Plots the summary effect
#'   size and 95% CI for both the distribution of raw effect sizes as well as
#'   the distribution of adjusted effect sizes.
#' @param method Defaults to FALSE, can also be 'thirds' for 16.65th, 50th, and
#'   83.35th percentiles, or 'quads' for 25th, 50th, and 75th percentiles.
#' @param abs Defaults to FALSE. If set to TRUE, plots all effect sizes as
#'    absolute values
#' @param sum_es_type Defaults to "diamond". Sets the type of summary effect
#'    size visualisation. "diamond" will create a diamond shape centred around
#'    the summary effect size, with the width of the diamond showing the 95% CI.
#'    Can also be set to "dot", which shows a dot centred on the summary effect
#'    size, with lines and brackets showing the width of the 95% CI.
#' @param bin_width Numeric argument that corresponds to the bin width for the
#'   histogram. Defaults to 0.1
#'
#' @return A ggplot object
#' @export
#'
#' @examples esd_plot_pba(l1, es_type = "Cohen's d", sum_es = TRUE)
esd_plot_pba <- function(lim_obj,
                         es_type,
                         sum_es = TRUE,
                         method = FALSE,
                         abs = FALSE,
                         sum_es_type = "diamond",
                         bin_width = 0.1) {

  df <- data.frame(lim_obj[1], # TE
                   lim_obj[3], # TE.limit
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
  if (isFALSE(abs)) {
    plot <- ggplot(data = df) +
      geom_histogram(aes(x = TE), fill="#dde7f0", binwidth = bin_width) +
      geom_histogram(aes(x = TE.limit, y = -..count..), fill = "#355C7D", binwidth = bin_width) +
      theme_minimal() +
      geom_hline(aes(yintercept = 0), size = 0.2)+
      labs(x = es_type, y = "Frequency")+
      theme(
        axis.text = element_text(size=12),
        axis.title = element_text(size=20))+
      scale_y_continuous(labels = function(x) abs(x))
  } else if (!isFALSE(abs)) {
    plot <- ggplot(data = df) +
      geom_histogram(aes(x = TE_abs), fill="#dde7f0", binwidth = bin_width, center = (bin_width / 2)) +
      geom_histogram(aes(x = TE.limit_abs, y = -..count..), fill= "#355C7D", binwidth = bin_width, center = (bin_width / 2)) +
      theme_minimal() +
      geom_hline(aes(yintercept = 0), size = 0.2)+
      labs(x = es_type, y = "Frequency")+
      theme(
        axis.text = element_text(size=12),
        axis.title = element_text(size=20))+
      scale_y_continuous(labels = function(x) abs(x))
  } else {
    return("No valid value for 'abs'")
  }

  if (!isFALSE(sum_es) & isFALSE(method)) {
    if (sum_es_type == "diamond") {
      diamond <- data.frame(
       x.random = c(df$lower.random[1], df$TE.random[1], df$upper.random[1], df$TE.random[1]),
       y.random = c(2, 1, 2, 3),
       x.adjust = c(df$lower.adjust[1], df$TE.adjust[1], df$upper.adjust[1], df$TE.adjust[1]),
       y.adjust = c(-2, -3, -2, -1)
       )

      plot <- plot+
       geom_polygon(data = diamond, aes(x = x.random, y = y.random), fill = "red") +
       geom_polygon(data = diamond, aes(x = x.adjust, y = y.adjust), fill = "#121F2B", size = 1)
    } else if (sum_es_type == "dot") {
      plot <- plot +
        geom_segment(aes(x = lower.random[1], xend = upper.random[1], y = 2, yend = 2), color = "#121F2B", linetype = "solid", size = 2)+
        geom_point(aes(x = TE.random[1], y = 2), size = 5, color = "#121F2B")+
        geom_point(aes(x = lower.random[1], y = 2), pch = "|", size = 5, color = "#121F2B")+
        geom_point(aes(x = upper.random[1], y = 2), pch = "|", size = 5, color = "#121F2B")+
        geom_segment(aes(x = lower.adjust[1], xend = upper.adjust[1], y = -2, yend = -2), color = "#121F2B", linetype = "solid", size = 2)+
        geom_point(aes(x = TE.adjust[1], y = -2), size = 5, color = "#121F2B")+
        geom_point(aes(x = lower.adjust[1], y = -2), pch = "|", size = 5, color = "#121F2B")+
        geom_point(aes(x = upper.adjust[1], y = -2), pch = "|", size = 5, color = "#121F2B")
    } else {
      return(warning("Please enter a valid "))
    }
  } else if (!isFALSE(method) & isFALSE(sum_es)) {
    if (method == "quads") {
      TE_q1 <- quantile(df$TE_abs, prob = 0.25)
      TEL_q1 <- quantile(df$TE.limit_abs, prob = 0.25)
      q1_label <- "25th"
      TE_q2 <- quantile(df$TE_abs, prob = 0.50)
      TEL_q2 <- quantile(df$TE.limit_abs, prob = 0.5)
      q2_label <- "50th"
      TE_q3 <- quantile(df$TE_abs, prob = 0.75)
      TEL_q3 <- quantile(df$TE.limit_abs, prob = 0.75)
      q3_label <- "75th"

      plot <- plot +
        geom_segment(aes(x = TE_q1, xend = TE_q1, y = 0, yend = Inf, color = "q1"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TEL_q1, xend = TEL_q1, y = 0, yend = -Inf, color = "q1"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TE_q2, xend = TE_q2, y = 0, yend = Inf, color = "q2"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TEL_q2, xend = TEL_q2, y = 0, yend = -Inf, color = "q2"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TE_q3, xend = TE_q3, y = 0, yend = Inf, color = "q3"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TEL_q3, xend = TEL_q3, y = 0, yend = -Inf, color = "q3"),
                     linetype = "dashed",
                     size = 1) +
        scale_color_manual(name = "Percentiles",
                           values = c(q1 = "#F8B195",
                                      q2 = "#F67280",
                                      q3 = "#C06C84"),
                           labels = c(q1 = q1_label,
                                      q2 = q2_label,
                                      q3 = q3_label))+
        theme(legend.position = c(0.9, 0.9),
              legend.background = element_rect(fill="#dde7f0",
                                               color = "#dde7f0"),
              legend.title = element_text(size=14),
              legend.text = element_text(size=12),
              legend.key.size = unit(1, 'cm'))

    } else if (method == "thirds") {
      q1 <- quantile(es_col, prob = 0.1665)
      q1_label <- "16.65th"
      q2 <- quantile(es_col, prob = 0.50)
      q2_label <- "50th"
      q3 <- quantile(es_col, prob = 0.8335)
      q3_label <- "83.35th"
      plot <- plot +
        geom_segment(aes(x = TE_q1, xend = TE_q1, y = 0, yend = Inf, color = "q1"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TEL_q1, xend = TEL_q1, y = 0, yend = -Inf, color = "q1"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TE_q2, xend = TE_q2, y = 0, yend = Inf, color = "q2"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TEL_q2, xend = TEL_q2, y = 0, yend = -Inf, color = "q2"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TE_q3, xend = TE_q3, y = 0, yend = Inf, color = "q3"),
                     linetype = "dashed",
                     size = 1) +
        geom_segment(aes(x = TEL_q3, xend = TEL_q3, y = 0, yend = -Inf, color = "q3"),
                     linetype = "dashed",
                     size = 1) +
        scale_color_manual(name = "Percentiles",
                           values = c(q1 = "#F8B195",
                                      q2 = "#F67280",
                                      q3 = "#C06C84"),
                           labels = c(q1 = q1_label,
                                      q2 = q2_label,
                                      q3 = q3_label))+
        theme(legend.position = c(0.9, 0.9),
              legend.background = element_rect(fill="#dde7f0",
                                               color = "#dde7f0"),
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
