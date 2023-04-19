#' Title
#'
#' @param df Dataset
#' @param es Column name of effect sizes
#' @param value The value of the effect size of interest
#'
#' @return A value which describes to which percentile in the ESD the inputted
#' value corresponds
#' @export
#'
#' @examples esd_perc(dat, yi, 0.2)
esd_perc <- function(df,
                     es,
                     value) {
    df <- as.data.frame(df)
    es_col <- df[, deparse(substitute(es))]
    perc <- length(es_col[es_col <= value])/length(es_col) * 100
    return(perc)
}
