#' Calculate the percentile of an ESD corresponding to a specific value
#'
#' @param df Dataset
#' @param es Column name of effect sizes
#' @param value The value of the effect size of interest
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
                     method = "exclusive") {
    df <- as.data.frame(df)
    es_col <- df[, deparse(substitute(es))]

    if (method == "exclusive") {
      rank <- length(es_col[es_col < value])
      perc <- rank/length(es_col) * 100
      return(perc)
    } else if (method == "inclusive") {
      rank <- length(es_col[es_col <= value])
      perc <- rank/length(es_col) * 100
      return(perc)
    } else if (method == "grouped") {
      rank <- length(es_col[es_col < value]) + (length(es_col[es_col == value]) / 2)
      perc <- rank/length(es_col) * 100
      return(perc)
    } else {
      return("please enter a valid method")
    }

}
