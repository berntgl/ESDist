#' Basic plot
#'
#' @param df A dataframe.
#' @param es The effect size column.
#' @param se The standard error column (defaults to NULL).
#' @param es_type A string specifying the type of effect size (for x-axis
#' label).
#' @param grouping_var Column name of grouping variable.
#' @param min_group_size Sets the minimum amount of effect sizes in each group
#' as specified by the `grouping_var` variable. Defaults to 20.
#' @param weighted Defaults to FALSE. If set to TRUE, will weight all effect
#' sizes by their standard error (requires `se` to be defined).
#' @param sesoi Defaults to NULL. When given a value, will calculate the
#' percentage of (weighted) effect sizes larger (or equal to) this value.
#' @param bin_width Width of the histogram bins. Defaults to 0.1.
#'
#' @returns A 'ggplot' object.
#' @noRd

basic_plot <- function(df,
                       es,
                       se = NULL,
                       es_type = "Effect size",
                       grouping_var = NULL,
                       min_group_size = 20,
                       weighted = FALSE,
                       sesoi = NULL,
                       bin_width = 0.1) {

  # Colour palette
  primary_darkest <- "#00161E"
  primary_dark <- "#07445B"
  primary_medium <- "#206985"
  primary_light <- "#C6DFE8"
  primary_lightest <- "#dde7f0"
  secondary_dark <- "#BBBBBB"
  secondary_light <- "#E7E7E7"
  accent <- "#D5A42C"



  if (!is.null(grouping_var)) {
    df <- df |>
      group_by(.data[[grouping_var]]) |>
      filter(n() >= 20) |>
      ungroup()
  }

  es_col <- df[[es]]


  # Calculate weights
  if (weighted) {
    stopifnot(!missing(se))
    se_col <- df[[se]]
    # Calculate the weights using the inverse of the variance (1 / (se_col^2))
    weights <- 1 / (se_col^2)

    y_label <- "Weighted count"

  } else {

    # If weighted = FALSE, set all weights to 1
    weights <- rep(1, length(es_col))
    y_label <- "Count"
  }

  # If smallest effect size of interest (sesoi) is NOT given, create a normal
  # plot
  if (is.null(sesoi)){
    plot <- ggplot(data = df) +
      geom_histogram(aes(.data[[es]], weight = weights),
                     fill = primary_dark,
                     binwidth = bin_width,
                     center = (bin_width / 2)) +
      labs(x = es_type, y = y_label) +
      theme_minimal()
  } else {

    # If sesoi is given, calculate the percentile of the sesoi value
    rank <- sum(weights[df[[es]] < sesoi]) / sum(weights) * 100

    rank_rev <- 100 - rank

    rank_perc <- sprintf("%.2f%%", rank)
    rank_rev_perc <- sprintf("%.2f%%", rank_rev)

    # Colour the range of effect sizes > sesoi in a different colour.
    plot <- ggplot(data = df) +
      geom_histogram(aes(.data[[es]], weight = weights, fill = after_stat(x) > sesoi),
                     binwidth = bin_width,
                     center = (bin_width / 2)) +
      scale_fill_manual(name = sprintf("ES < or > %.2f", sesoi),
                        labels = c(rank_perc, rank_rev_perc),
                        values = c(secondary_light, primary_dark)) +
      labs(x = es_type, y = y_label)+
      theme_minimal()+
      guides(fill = guide_legend(position = "inside")) +
      theme(legend.position.inside = c(0.9, 0.7),
            legend.background = element_rect(fill=primary_lightest, color = primary_lightest),
            legend.title = element_text(size=14),
            legend.text = element_text(size=12),
            legend.key.size = unit(1, 'cm'),
            axis.text = element_text(size=12),
            axis.title = element_text(size=20))
  }
  if (!is.null(grouping_var)) {
    plot <- plot +
      facet_grid(vars(.data[[grouping_var]]),
                 switch = "y")
  }
  return(plot)
}



#' Iceberg plot.
#'
#' @param df A dataframe.
#' @param es_random The column containing the raw effect sizes.
#' @param es_adjust The column containing the publication bias-adjusted effect
#' sizes.
#' @param se_random Column containing the standard errors of the raw effect
#' sizes.
#' @param se_adjust Column containing the standard errors of the publication
#' bias-adjusted effect sizes.
#' @param es_type A string specifying the type of effect size (for x-axis
#' label).
#' @param weighted Defaults to FALSE. If set to TRUE, will weight all effect
#' sizes by their standard error (requires `se_random` and `se_adjusted` to be
#' defined).
#' @param sesoi Defaults to NULL. When given a value, will calculate the
#' percentage of (weighted) effect sizes larger (or equal to) this value.
#' @param bin_width Width of the histogram bins. Defaults to 0.1.
#'
#' @returns A 'ggplot' object.
#' @noRd

iceberg_plot <- function(df,
                         es_random = "TE",
                         es_adjust = "TE.limit",
                         se_random = "seTE",
                         se_adjust = "seTE.limit",
                         es_type = "Effect size",
                         weighted  = FALSE,
                         sesoi = NULL,
                         bin_width = 0.1) {
  # Colour palette
  primary_darkest <- "#00161E"
  primary_dark <- "#07445B"
  primary_medium <- "#206985"
  primary_light <- "#C6DFE8"
  primary_lightest <- "#dde7f0"
  secondary_dark <- "#BBBBBB"
  secondary_light <- "#E7E7E7"
  accent <- "#D5A42C"

  es_rand_col   <- df[[es_random]]
  es_adj_col    <- df[[es_adjust]]

  # If weighted: compute weights from SE; otherwise set to 1
  if (weighted) {
    stopifnot(!is.null(se_random), !is.null(se_adjust))

    se_rand_col <- df[[se_random]]
    se_adj_col  <- df[[se_adjust]]

    df$weights        <- 1 / (se_rand_col^2)
    df$weights.limit  <- 1 / (se_adj_col^2)

    y_label <- "Weighted count"
  } else {
    df$weights        <- 1
    df$weights.limit  <- 1
    y_label <- "Count"
  }

  # If sesoi is missing, just draw the two histograms without colouring by > sesoi
  if (is.null(sesoi)) {
    plot <- ggplot(data = df) +
      geom_histogram(aes(x = .data[[es_random]],
                         weight = weights),
                     fill = primary_light,
                     binwidth = bin_width,
                     center = (bin_width / 2)) +
      geom_histogram(aes(x = .data[[es_adjust]],
                         y = -after_stat(count),
                         weight = weights.limit),
                     fill = primary_medium,
                     binwidth = bin_width,
                     center = (bin_width / 2)) +
      theme_minimal() +
      geom_hline(aes(yintercept = 0), size = 0.2) +
      labs(x = es_type, y = y_label) +
      scale_y_continuous(labels = function(x) abs(x)) +
      theme(axis.text  = element_text(size = 12),
            axis.title = element_text(size = 20))

    return(plot)
  }

  # Ranks (with or without weights)

  if (isTRUE(weighted)) {
    rank_random <- sum(df$weights[es_rand_col < sesoi]) /
      sum(df$weights) * 100
    rank_adjust <- sum(df$weights.limit[es_adj_col < sesoi]) /
      sum(df$weights.limit) * 100
  } else {
    rank_random <- length(es_rand_col[es_rand_col < sesoi]) /
      length(es_rand_col) * 100
    rank_adjust <- length(es_adj_col[es_adj_col < sesoi]) /
      length(es_adj_col) * 100
  }

  rank_rev_random  <- 100 - rank_random
  rank_rev_adjust  <- 100 - rank_adjust

  rank_perc_random     <- sprintf("%.2f%%", rank_random)
  rank_rev_perc_random <- sprintf("%.2f%%", rank_rev_random)
  rank_perc_adjust     <- sprintf("%.2f%%", rank_adjust)
  rank_rev_perc_adjust <- sprintf("%.2f%%", rank_rev_adjust)

  # Plot

  plot <- ggplot(data = df) +
    geom_histogram(
      aes(x = .data[[es_random]],
          weight = weights,
          fill = ifelse(after_stat(x) > sesoi, "B", "A")),
      binwidth = bin_width,
      center = (bin_width / 2)
    ) +
    geom_histogram(
      aes(x = .data[[es_adjust]],
          y = -after_stat(count),
          weight = weights.limit,
          fill = ifelse(after_stat(x) > sesoi, "D", "C")),
      binwidth = bin_width,
      center = (bin_width / 2)
    ) +
    theme_minimal() +
    geom_hline(aes(yintercept = 0), size = 0.2) +
    scale_fill_manual(
      name   = sprintf("ES < or > %.2f", sesoi),
      labels = c(
        "A" = rank_perc_random,
        "B" = rank_rev_perc_random,
        "C" = rank_perc_adjust,
        "D" = rank_rev_perc_adjust
      ),
      values = c(
        "A" = secondary_light,
        "B" = primary_light,
        "C" = secondary_dark,
        "D" = primary_medium
      )
    ) +
    labs(x = es_type, y = y_label) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 14, hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.key.size = unit(1, "cm"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 20)
    ) +
    scale_y_continuous(labels = function(x) abs(x))

  return(plot)
}


#' Add benchmarks to plot.
#'
#' @param plot The plot object from the parent function.
#' @param df A dataframe.
#' @param es The effect size column.
#' @param se The standard error column (defaults to NULL).
#' @param grouping_var Column name of grouping variable.
#' @param min_group_size Sets the minimum amount of effect sizes in each group
#' as specified by the `grouping_var` variable. Defaults to 20.
#' @param weighted Defaults to FALSE. If set to TRUE, will weight all effect
#' sizes by their standard error (requires `se` to be defined).
#' @param method Defaults to NULL. Can be set to "quads" (.25, .5, .75) or
#' "thirds" (.1665, .5, .8335) to plot vertical lines based on the respective
#'  benchmarks.
#' @param ci Defaults to FALSE. If set to TRUE, will plot 95% CIs for each
#' benchmark (requires `method` to be defined).
#' @param mirrored Defaults to FALSE. When set to TRUE, will flip the
#' benchmark lines to range from -Inf to 0.
#'
#' @returns A 'ggplot' object.
#' @noRd

add_benchmarks <- function(plot,
                           df,
                           es,
                           se = NULL,
                           grouping_var = NULL,
                           min_group_size = 20,
                           weighted = FALSE,
                           method,
                           ci = FALSE,
                           n_bootstrap = 1000,
                           mirrored = FALSE) {

  primary_lightest <- "#dde7f0"
  benchmarks1 <- "#FF8C77"
  benchmarks2 <- "#D5462C"
  benchmarks3 <- "#921B05"

  if (method == "quads") {
    probs  <- c(0.25, 0.5, 0.75)
    labels <- c("25%", "50%", "75%")
  } else if (method == "thirds") {
    probs  <- c(0.1665, 0.5, 0.8335)
    labels <- c("16.65%", "50%", "83.35%")
  } else {
    stop("Unknown method: ", method)
  }

  if (mirrored) {
    y_start <- -Inf
    y_end   <- 0
  } else {
    y_start <- 0
    y_end   <- Inf
  }

  # Helper: compute annotation for a single data frame (one group)
  make_annotation_ci <- function(d) {
    qt <- calculate_percentiles_ci(df = d,
                                   es = es,
                                   se = se,
                                   weighted = weighted,
                                   probs = probs,
                                   n_bootstrap = n_bootstrap)

    data.frame(
      q     = c(as.numeric(unname(qt[1])),
                as.numeric(unname(qt[4])),
                as.numeric(unname(qt[7]))),
      lower = c(as.numeric(unname(qt[2])),
                as.numeric(unname(qt[5])),
                as.numeric(unname(qt[8]))),
      upper = c(as.numeric(unname(qt[3])),
                as.numeric(unname(qt[6])),
                as.numeric(unname(qt[9]))),
      label = labels
    )
  }

  make_annotation_no_ci <- function(d) {
    qt <- calculate_percentiles(df = d,
                                es = es,
                                se = se,
                                weighted = weighted,
                                probs = probs)

    data.frame(
      q     = c(as.numeric(unname(qt[1])),
                as.numeric(unname(qt[2])),
                as.numeric(unname(qt[3]))),
      label = labels
    )
  }

  if (is.null(grouping_var)) {

    # No grouping
    if (ci) {
      annotation <- make_annotation_ci(df)
      plot <- plot +
        geom_rect(data = annotation,
                  aes(xmin = lower, xmax = upper,
                      ymin = y_start, ymax = y_end),
                  fill = benchmarks1,
                  color = NA,
                  alpha = 0.3,
                  size = 1)
    } else {
      annotation <- make_annotation_no_ci(df)
    }

  } else {

    if (ci) {
      annotation <- df |>
        group_by(.data[[grouping_var]]) |>
        filter(n() >= min_group_size) |>
        group_modify(~ make_annotation_ci(.x)) |>
        ungroup()
      names(annotation)[1] <- grouping_var

      plot <- plot +
        geom_rect(data = annotation,
                  aes(xmin = lower, xmax = upper,
                      ymin = y_start, ymax = y_end),
                  fill = benchmarks1,
                  color = NA,
                  alpha = 0.3,
                  size = 1)
    } else {
      annotation <- df |>
        group_by(.data[[grouping_var]]) |>
        filter(n() >= 20) |>
        group_modify(~ make_annotation_ci(.x)) |>
        ungroup()
      names(annotation)[1] <- grouping_var
    }
  }

  # Add vertical percentile lines (works for both grouped / ungrouped)
  plot <- plot +
    geom_segment(data = annotation,
                 aes(x = q,
                     y = y_start,
                     yend = y_end,
                     color = label),
                 linetype = "dashed",
                 linewidth = 1) +
    theme(legend.position = c(0.9, 0.7),
          legend.background = element_rect(fill = primary_lightest,
                                           color = primary_lightest)) +
    scale_color_manual(name = "Percentiles",
                       values = c(benchmarks1, benchmarks2, benchmarks3))

  return(plot)
}
