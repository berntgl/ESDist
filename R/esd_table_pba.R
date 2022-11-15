#' Adjusting an ESD for publication bias
#'
#' @param df Dataset
#' @param es The name of the column containing the effect sizes
#' @param adj_es Adjusted effect size. Can be a single number, or a vector of
#' numbers that correspond to the adjusted effect sizes for each group in
#' alphabetical order, with the overall adjusted effect size at the end.
#' @param grouping_var Column name of grouping variable
#' @param method Defaults to 'quads', can also be 'thirds'
#' @param csv_write Defaults to FALSE. Will write the outputted table as a csv
#' when set to TRUE.
#'
#' @return a matrix
#' @export
#'
#' @examples
#' esd_table_pba(df, es, 0.2, grouping_var = group, method = "thirds")
#'
esd_table_pba <- function(df, es, adj_es, grouping_var = NULL, method = "quads", csv_write = FALSE) {
  if(missing(grouping_var)) {
    es_median <- median(df[, deparse(substitute(es))], na.rm = TRUE)

    if(method == "thirds") {
      adj_values <- df %>%
        summarise(q16 = quantile({{ es }}, prob = .1665, na.rm = TRUE),
                  q16_adj = cdq16  - 0.698 * (es_median - adj_es),
                  q50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  q50_adj = adj_es,
                  q83 = quantile({{ es }}, prob = .8335, na.rm = TRUE),
                  q83_adj = cdq83 - ((es_median - adj_es)/0.903),
                  count = n())
    } else if (method == "quads") {
      adj_values <- df %>%
        summarise(q25 = quantile({{ es }}, prob = .25, na.rm = TRUE),
                  q25_adj = cdq25 - 0.718 * (es_median - adj_es),
                  q50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  q50_adj = adj_es,
                  q75 = quantile({{ es }}, prob = .75, na.rm = TRUE),
                  q75_adj = cdq75 - ((es_median - adj_es)/0.895),
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


    if (method == "thirds") {
      adj_values <- df %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq16 = quantile({{ es }}, prob = .1665, na.rm = TRUE),
                  q16_adj = q16  - 0.698 * (es_median[as.numeric(cur_group_id())] - adj_es[as.numeric(cur_group_id())]),
                  q50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  q50_adj = adj_es[as.numeric(cur_group_id())],
                  q83 = quantile({{ es }}, prob = .8335, na.rm = TRUE),
                  q83_adj = q83 - ((es_median[as.numeric(cur_group_id())] - adj_es[as.numeric(cur_group_id())])/0.903),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>%
                    summarise({{grouping_var}} := "All",
                              q16 = quantile({{ es }}, prob = .1665, na.rm = TRUE),
                              q16_adj = q16  - 0.698 * (es_median[as.numeric(length(es_median))] - adj_es[as.numeric(length(adj_es))]),
                              q50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                              q50_adj = adj_es[as.numeric(length(adj_es))],
                              q83 = quantile({{ es }}, prob = .8335, na.rm = TRUE),
                              q83_adj = q83 - ((es_median[as.numeric(length(es_median))] - adj_es[as.numeric(length(adj_es))])/0.903),
                              count = n()))

    } else if (method == "quads") {
      adj_values <- df %>%
        group_by({{ grouping_var }}) %>%
        summarise(q25 = quantile({{ es }}, prob = .25, na.rm = TRUE),
                  q25_adj = q25  - 0.718 * (es_median[as.numeric(cur_group_id())] - adj_es[as.numeric(cur_group_id())]),
                  q50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  q50_adj = adj_es[as.numeric(cur_group_id())],
                  q75 = quantile({{ es }}, prob = .75, na.rm = TRUE),
                  q75_adj = q75 - ((es_median[as.numeric(cur_group_id())] - adj_es[as.numeric(cur_group_id())])/0.895),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise({{grouping_var}} := "All",
                                   q25 = quantile({{ es }}, prob = .25, na.rm = TRUE),
                                   q25_adj = q25  - 0.718 * (es_median[as.numeric(length(es_median))] - adj_es[as.numeric(length(adj_es))]),
                                   q50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                                   q50_adj = adj_es[as.numeric(length(adj_es))],
                                   q75 = quantile({{ es }}, prob = .75, na.rm = TRUE),
                                   q75_adj = q75 - ((es_median[as.numeric(length(es_median))] - adj_es[as.numeric(length(adj_es))])/0.895),
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
