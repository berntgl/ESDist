pb_adj <- function(df, es, adj_es, grouping_var = NULL, method = "quads", csv_write = FALSE) {
  if(missing(grouping_var)) {
    es_median <- median(df[, deparse(substitute(es))], na.rm = TRUE)
    adj_ratio <- adj_es / es_median

    if(method == "thirds") {
      adj_values <- df %>%
        summarise(cdq16 = quantile({{ es }}, prob = .1665, na.rm = TRUE),
                  cdq16_adj = cdq16 * adj_ratio * 3.42 * (es_median - cdq16),
                  cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  cdq50_adj = adj_es,
                  cdq83 = quantile({{ es }}, prob = .8335, na.rm = TRUE),
                  cdq83_adj = cdq83 * adj_ratio * 5.59 * (cdq83 - es_median),
                  count = n())
    } else if (method == "quads") {
      adj_values <- df %>%
        summarise(cdq25 = quantile({{ es }}, prob = .25, na.rm = TRUE),
                  cdq25_adj = cdq25 * adj_ratio * 6.43 * (es_median - cdq25),
                  cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  cdq50_adj = adj_es,
                  cdq75 = quantile({{ es }}, prob = .75, na.rm = TRUE),
                  cdq75_adj = cdq75 * adj_ratio * 7.59 * (cdq75 - es_median),
                  count = n())
    } else {
      return("Please enter a valid method")
    }

    adj_values <- as.matrix(adj_values)
    adj_table <- matrix(nrow = 2, ncol = 4)
    rownames(adj_table) <- c("Raw effect size", "Adjusted effect size")

    for (i in 1:2) {
      for (j in 1:3) {
        index <- (i-1+(j*2-1))
        adj_table[i,j] <- adj_values[index]
      }
      adj_table[i,4] <- adj_values[7]
    }
  } else {
    es_median <- c(as.vector(by(df[,deparse(substitute(es))],
                                df[,deparse(substitute(grouping_var))],
                                median)),
                   median(df[,deparse(substitute(es))]))

    adj_ratio <- (es_median - adj_es) / es_median


    if (method == "thirds") {
      adj_values <- df %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq16 = quantile({{ es }}, prob = .1665, na.rm = TRUE),
                  cdq16_adj = cdq16 - (1.36 * adj_ratio[as.numeric(cur_group_id())] * cdq16),
                  cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  cdq50_adj = adj_es[as.numeric(cur_group_id())],
                  cdq83 = quantile({{ es }}, prob = .8335, na.rm = TRUE),
                  cdq83_adj = cdq83 - (0.641 * adj_ratio[as.numeric(cur_group_id())] * cdq83),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>%
                    summarise({{grouping_var}} := "All",
                              cdq16 = quantile({{ es }}, prob = .1665, na.rm = TRUE),
                              cdq16_adj = cdq16 - (1.36 * adj_ratio[as.numeric(length(adj_es))] * cdq16),
                              cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                              cdq50_adj = adj_es[as.numeric(length(adj_es))],
                              cdq83 = quantile({{ es }}, prob = .8335, na.rm = TRUE),
                              cdq83_adj = cdq83 - (0.641 * adj_ratio[as.numeric(length(adj_es))] * cdq83),
                              count = n()))

    } else if (method == "quads") {
      adj_values <- df %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq25 = quantile({{ es }}, prob = .25, na.rm = TRUE),
                  cdq25_adj = cdq25 - (1.14 * adj_ratio[as.numeric(cur_group_id())] * cdq25),
                  cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  cdq50_adj = adj_es[as.numeric(cur_group_id())],
                  cdq75 = quantile({{ es }}, prob = .75, na.rm = TRUE),
                  cdq75_adj = cdq75 - (0.752 * adj_ratio[as.numeric(cur_group_id())] * cdq75),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise({{grouping_var}} := "All",
                                   cdq25 = quantile({{ es }}, prob = .25, na.rm = TRUE),
                                   cdq25_adj = cdq25 - (1.14 * adj_ratio[as.numeric(length(adj_es))] * cdq25),
                                   cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                                   cdq50_adj = adj_es[as.numeric(length(adj_es))],
                                   cdq75 = quantile({{ es }}, prob = .75, na.rm = TRUE),
                                   cdq75_adj = cdq75 - (0.752 * adj_ratio[as.numeric(length(adj_es))] * cdq75),
                                   count = n()))
    } else {
      return("please enter a valid method")
    }
    adj_values <- as.matrix(adj_values)

    adj_table <- matrix(nrow = 2 * nrow(adj_values), ncol = 4)
    adj_table_rownames <- c()

    for (h in 1:nrow(adj_values)) {
      adj_table_rownames[h*2-1] <- as.character(adj_values[h,1])
      adj_table_rownames[h*2] <- as.character(paste(adj_values[h,1], "adjusted", sep = " "))
      for (i in 1:2) {
        row_index <- i-1+(h*2-1)
        for (j in 1:3) {
          col_index <- (i+(j*2)-1)
          adj_table[row_index,j] <- format(round(as.numeric(adj_values[h,col_index]), 3), nsmall = 3)
        }
        adj_table[row_index,4] <- adj_values[h,8]
      }
    }
    rownames(adj_table) <- adj_table_rownames
  }
  ifelse(method == "thirds",
         colnames(adj_table) <- c("16.65%", "50%", "83.35%", "Number of effects"),
         colnames(adj_table) <- c("25%", "50%", "75%", "Number of effects"))
  if (csv_write == TRUE) {
    write.csv(adj_table, file = "pb_adj_2_table.csv")
  }
  return(adj_table)
}
