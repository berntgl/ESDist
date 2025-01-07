#' Calculate the percentile of an ESD corresponding to a specific value
#'
#' @param df Dataset
#' @param es Column name of effect sizes
#' @param value The value of the effect size of interest
#' @param se Column name of standard error.
#' @param weighted Defaults to FALSE. When set to TRUE, will calculate the
#'  weighted distribution based on the inverse standard error.
#' @param method Specifies method, defaults to 'exclusive' but can also be
#' 'inclusive' or 'grouped'.
#'
#' @return A value which describes to which percentile in the ESD the inputted
#' value corresponds
#' @export
#'
#' @examples esd_perc(ot_dat, yi, 0.2)
esd_perc <- function(df,
                     es,
                     value,
                     se = NULL,
                     weighted = FALSE,
                     method = "exclusive") {
    df <- as.data.frame(df)
    df$es_col_abs <- abs(df[, deparse(substitute(es))])

    if (isTRUE(weighted)) {
      stopifnot(!missing(se))
      df$se <- df[, deparse(substitute(se))]
      df$weights <- 1 / df$se
    } else {
      df$weights <- 1
    }

    if (method == "exclusive") {
      rank <- sum(df$weights[df$es_col_abs < value])
      perc <- rank / sum(df$weights) * 100
      return(perc)
    } else if (method == "inclusive") {
      rank <- sum(df$weights[df$es_col_abs <= value])
      perc <- rank / sum(df$weights) * 100
      return(perc)
    } else if (method == "grouped") {
      rank <- sum(df$weights[df$es_col_abs < value]) + (sum(df$weights[df$es_col_abs == value]) / 2)
      perc <- rank / sum(df$weights) * 100
      return(perc)
    } else {
      return("please enter a valid method")
    }
}
