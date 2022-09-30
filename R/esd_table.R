esd_table <- function(df, cohensd, grouping_var = NULL, method = "quads", csv_write = FALSE) {
  if(missing(grouping_var)) {
    if(method == "quads") {
      table <- df %>%
        summarise(cdq25 = quantile({{ cohensd }}, prob = .25),
                  cdq50 = quantile({{ cohensd }}, prob = .50),
                  cdq75 = quantile({{ cohensd }}, prob = .75),
                  count = n())
      return(table)
    } else if (method == "thirds") {
      table <- df %>%
        summarise(cdq16 = quantile({{ cohensd }}, prob = .1665),
                  cdq50 = quantile({{ cohensd }}, prob = .50),
                  cdq83 = quantile({{ cohensd }}, prob = .8335),
                  count = n())
      return(table)
    } else {
      return("Please enter a valid method")
    }
  } else {
    if (method == "quads") {
      table <- df %>%
        mutate({{grouping_var}} := as.character({{grouping_var}})) %>%
        bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq25 = quantile({{ cohensd }}, prob = .25),
                  cdq50 = quantile({{ cohensd }}, prob = .50),
                  cdq75 = quantile({{ cohensd }}, prob = .75),
                  count = n()) %>%
        arrange(match(group, "All"), group)
    } else if (method == "thirds") {
      table <- df %>%
        mutate({{grouping_var}} := as.character({{grouping_var}})) %>%
        bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq16 = quantile({{ cohensd }}, prob = .1665),
                  cdq50 = quantile({{ cohensd }}, prob = .50),
                  cdq83 = quantile({{ cohensd }}, prob = .8335),
                  count = n()) %>%
        arrange(match(group, "All"), group)
    } else {
      return("Please enter a valid method")
    }
  }
  if (csv_write == TRUE) {
    write.csv(table, file = "esd_table.csv")
  }
  table
}
