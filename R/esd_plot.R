esd_plot <- function(df, cohensd) {
  cdq25 <- quantile({{ cohensd }}, prob = 0.25)
  cdq50 <- quantile({{ cohensd }}, prob = 0.50)
  cdq75 <- quantile({{ cohensd }}, prob = 0.75)

  ggplot(data = df, aes({{ cohensd }})) + geom_histogram(binwidth = .25) +
    geom_vline(aes(xintercept = cdq25), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = cdq50), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = cdq75), color = "red", linetype = "dashed", size = 1) +
    scale_x_continuous(breaks = seq(0, 3, 0.5)) +
    labs(x = "Cohen's d", y = "Frequency") +
    theme(axis.title.x = element_text(face = "bold", size = 20),
          axis.text.x  = element_text(size = 16),
          axis.title.y = element_text(face = "bold", size = 20),
          axis.text.y  = element_text(size = 16))
}
