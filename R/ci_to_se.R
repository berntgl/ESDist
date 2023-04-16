#' Converting 95% CI to SE
#'
#' @param lbound lower bound of 95% confidence interval
#' @param ubound upper bound of 95% confidence interval
#'
#' @return standard error
#' @export
#'
#' @examples
#' ci_to_se(-0.12, 1.71)

ci_to_se <- function(lbound, ubound) {
  se <- ((ubound) - (lbound)) / (2 * 1.96)
}
