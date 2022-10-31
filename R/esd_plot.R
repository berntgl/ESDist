esd_plot <- function(df, es, es_type, method = "quads", bin_width = 0.05) {
  es_col <- df[, deparse(substitute(es))]
  if (method == "quads") {
    cdq1 <- quantile(es_col, prob = 0.25)
    cdq1_label <- "25%"
    cdq2 <- quantile(es_col, prob = 0.50)
    cdq2_label <- "50%"
    cdq3 <- quantile(es_col, prob = 0.75)
    cdq3_label <- "75%"
  } else if (method == "thirds") {
    cdq1 <- quantile({{ es }}, prob = 0.1665)
    cdq1_label <- "16.65%"
    cdq2 <- quantile({{ es }}, prob = 0.50)
    cdq1_label <- "50%"
    cdq3 <- quantile({{ es }}, prob = 0.8335)
    cdq1_label <- "83.35%"
  } else {
    return("Please enter a valid method")
  }
  ggplot(data = df, aes(es_col)) +
    geom_histogram(fill = "#0C6170", binwidth = bin_width) +
    geom_vline(aes(xintercept = cdq1), color = "#A4E5E0", linetype = "dashed", size = 1) +
    annotate("text", x = (cdq1 + 0.05), y = 2, color = "#A4E5E0", label = cdq1_label, angle = 90) +
    geom_vline(aes(xintercept = cdq2), color = "#A4E5E0", linetype = "dashed", size = 1) +
    annotate("text", x = (cdq2 + 0.05), y = 2, color = "#A4E5E0", label = cdq2_label, angle = 90) +
    geom_vline(aes(xintercept = cdq3), color = "#A4E5E0", linetype = "dashed", size = 1) +
    annotate("text", x = (cdq3 + 0.05), y = 2, color = "#A4E5E0", label = cdq3_label, angle = 90) +
    scale_x_continuous(breaks = seq(0, 3, 0.5)) +
    labs(x = es_type, y = "Frequency") +
    theme(axis.title.x = element_text(face = "bold", size = 20),
          axis.text.x  = element_text(size = 16),
          axis.title.y = element_text(face = "bold", size = 20),
          axis.text.y  = element_text(size = 16))
}
