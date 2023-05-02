#' Creating a table for an effect size distribution
#'
#' @param df Dataset
#' @param es Column name of effect sizes
#' @param grouping_var Column name of grouping variable
#' @param method Defaults to 'quads', can also be 'thirds'
#' @param min_group_size Sets the minimum amount of effect sizes needed to
#' include a group in the table. Defaults to 3.
#' @param csv_write Defaults to FALSE. Will write the outputted table as a csv
#' @param path_file_name A string containing the directory to which the .csv
#' file will be saved, including the title of the .csv file (has to end in
#' '.csv')
#' @param ndec The number of decimal places in which all values should be
#' reported. Defaults to 2.
#' when set to TRUE.
#' @return a table
#' @export
#'
#' @examples
#' esd_table(ot_dat, yi_abs, grouping_var = group, method = "thirds")
#'
#'
esd_table <- function(df,
                      es,
                      grouping_var = NULL,
                      method = "quads",
                      min_group_size = 3,
                      csv_write = FALSE,
                      path_file_name = "esd_table.csv",
                      ndec = 2) {
  df <- as.data.frame(df)
  if(missing(grouping_var)) {
    if(method == "quads") {
      es_values <- df %>%
        summarise(cdq25 = quantile({{ es }}, prob = .25, na.rm = TRUE),
                  cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  cdq75 = quantile({{ es }}, prob = .75, na.rm = TRUE),
                  count = n())
    } else if (method == "thirds") {
      es_values <- df %>%
        summarise(cdq16 = quantile({{ es }}, prob = .1665, na.rm = TRUE),
                  cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  cdq83 = quantile({{ es }}, prob = .8335, na.rm = TRUE),
                  count = n())
    } else {
      return("Please enter a valid method")
    }
    es_values <- as.matrix(es_values)
    es_table <- matrix(nrow = 1, ncol = 4)
    rownames(es_table) <- "Raw effect size"


    for (i in 1:3) {
      es_table[i] <- format(round(as.numeric(es_values[i]), ndec), nsmall = ndec)
    }
    es_table[1,4] <- as.numeric(es_values[1,4])

  } else {
    if (method == "quads") {
      es_values <- df %>%
        # mutate({{ grouping_var }} := as.character({{ grouping_var }})) %>%
        # bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq25 = quantile({{ es }}, prob = .25, na.rm = TRUE),
                  cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  cdq75 = quantile({{ es }}, prob = .75, na.rm = TRUE),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise({{grouping_var}} := "All",
                                   cdq25 = quantile({{ es }}, prob = .25, na.rm = TRUE),
                                   cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                                   cdq75 = quantile({{ es }}, prob = .75, na.rm = TRUE),
                                   count = n()))
    } else if (method == "thirds") {
      es_values <- df %>%
        # mutate({{grouping_var}} := as.character({{grouping_var}})) %>%
        # bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq16 = quantile({{ es }}, prob = .1665, na.rm = TRUE),
                  cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                  cdq83 = quantile({{ es }}, prob = .8335, na.rm = TRUE),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise({{grouping_var}} := "All",
                                   cdq16 = quantile({{ es }}, prob = .1665, na.rm = TRUE),
                                   cdq50 = quantile({{ es }}, prob = .50, na.rm = TRUE),
                                   cdq83 = quantile({{ es }}, prob = .8335, na.rm = TRUE),
                                   count = n()))
    } else {
      return("Please enter a valid method")
    }

    es_values <- es_values %>%
      filter(count >= min_group_size)
    es_values <- as.matrix(es_values)
    es_table <- matrix(nrow = nrow(es_values), ncol = 4)
    rownames(es_table) <- as.character(es_values[,1])
    for (i in 1:nrow(es_values)) {
      for (j in 1:3) {
        es_table[i,j] <- format(round(as.numeric(es_values[i, j+1]), ndec), nsmall = ndec)
      }
      es_table[i,4] <- as.numeric(es_values[i,5])
    }
  }
  ifelse(method == "thirds",
         colnames(es_table) <- c("16.65%", "50%", "83.35%", "Number of effects"),
         colnames(es_table) <- c("25%", "50%", "75%", "Number of effects"))


  if (csv_write == TRUE) {
    write.csv(es_table, file = path_file_name)
  }
  return(es_table)
}
