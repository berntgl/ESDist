esd_table <- function(df, cohensd, grouping_var = NULL, method = "quads", csv_write = FALSE) {
  if(missing(grouping_var)) {
    if(method == "quads") {
      es_values <- df %>%
        summarise(cdq25 = quantile({{ cohensd }}, prob = .25, na.rm = TRUE),
                  cdq50 = quantile({{ cohensd }}, prob = .50, na.rm = TRUE),
                  cdq75 = quantile({{ cohensd }}, prob = .75, na.rm = TRUE),
                  count = n())
    } else if (method == "thirds") {
      es_values <- df %>%
        summarise(cdq16 = quantile({{ cohensd }}, prob = .1665, na.rm = TRUE),
                  cdq50 = quantile({{ cohensd }}, prob = .50, na.rm = TRUE),
                  cdq83 = quantile({{ cohensd }}, prob = .8335, na.rm = TRUE),
                  count = n())
    } else {
      return("Please enter a valid method")
    }
    es_values <- as.matrix(es_values)
    es_table <- matrix(nrow = 1, ncol = 4)
    rownames(es_table) <- "Raw effect size"


    for (i in 1:3) {
      es_table[i] <- format(round(as.numeric(es_values[i]), 3), nsmall = 3)
    }
    es_table[1,4] <- as.numeric(es_values[1,4])

  } else {
    if (method == "quads") {
      es_values <- df %>%
        # mutate({{ grouping_var }} := as.character({{ grouping_var }})) %>%
        # bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq25 = quantile({{ cohensd }}, prob = .25, na.rm = TRUE),
                  cdq50 = quantile({{ cohensd }}, prob = .50, na.rm = TRUE),
                  cdq75 = quantile({{ cohensd }}, prob = .75, na.rm = TRUE),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise({{grouping_var}} := "All",
                                   cdq25 = quantile({{ cohensd }}, prob = .25, na.rm = TRUE),
                                   cdq50 = quantile({{ cohensd }}, prob = .50, na.rm = TRUE),
                                   cdq75 = quantile({{ cohensd }}, prob = .75, na.rm = TRUE),
                                   count = n()))
    } else if (method == "thirds") {
      es_values <- df %>%
        # mutate({{grouping_var}} := as.character({{grouping_var}})) %>%
        # bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq16 = quantile({{ cohensd }}, prob = .1665, na.rm = TRUE),
                  cdq50 = quantile({{ cohensd }}, prob = .50, na.rm = TRUE),
                  cdq83 = quantile({{ cohensd }}, prob = .8335, na.rm = TRUE),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise({{grouping_var}} := "All",
                                   cdq16 = quantile({{ cohensd }}, prob = .1665, na.rm = TRUE),
                                   cdq50 = quantile({{ cohensd }}, prob = .50, na.rm = TRUE),
                                   cdq83 = quantile({{ cohensd }}, prob = .8335, na.rm = TRUE),
                                   count = n()))
    } else {
      return("Please enter a valid method")
    }


    es_values <- as.matrix(es_values)
    es_table <- matrix(nrow = nrow(es_values), ncol = 4)
    rownames(es_table) <- as.character(es_values[,1])
    for (i in 1:nrow(es_values)) {
      for (j in 1:3) {
        es_table[i,j] <- format(round(as.numeric(es_values[i, j+1]), 3), nsmall = 3)
      }
      es_table[i,4] <- as.numeric(es_values[i,5])
    }
  }
  ifelse(method == "thirds",
         colnames(es_table) <- c("16.65%", "50%", "83.35%", "Number of effects"),
         colnames(es_table) <- c("25%", "50%", "75%", "Number of effects"))


  if (csv_write == TRUE) {
    write.csv(es_table, file = "esd_table.csv")
  }
  return(es_table)
}
