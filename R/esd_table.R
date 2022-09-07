esd_table <- function(df, grouping_var, cohensd) {
  table <- df %>%
    group_by({{ grouping_var }}) %>%
    summarise(cdq25 = quantile({{ cohensd }}, prob = .25),
              cdq50 = quantile({{ cohensd }}, prob = .50),
              cdq75 = quantile({{ cohensd }}, prob = .75),)

  return(table)
}
