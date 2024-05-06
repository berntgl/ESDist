#' Creating a table for field-specific effect size benchmarks
#'
#' @param df Dataset.
#' @param es Column name of effect sizes.
#' @param grouping_var Column name of grouping variable.
#' @param method Defaults to 'quads', can also be 'thirds'.
#' @param min_group_size Sets the minimum amount of effect sizes needed to
#' include a group in the table. Defaults to 3.
#' @param csv_write Defaults to FALSE. Will write the outputted table as a csv.
#' @param path_file_name A string containing the directory to which the .csv
#' file will be saved, including the title of the .csv file (has to end in
#' '.csv').
#' @param ndec The number of decimal places in which all values should be
#' reported. Defaults to 2.
#' @return A table.
#' @export
#'
#' @examples
#' esd_table(ot_dat, yi, grouping_var = group, method = "thirds")
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
  df$es <- abs(df[, deparse(substitute(es))])
  if(missing(grouping_var)) {
    if(method == "quads") {
      es_values <- df %>%
        summarise(cdq25 = round(quantile(es, prob = .25, na.rm = TRUE), ndec),
                  cdq50 = round(quantile(es, prob = .50, na.rm = TRUE), ndec),
                  cdq75 = round(quantile(es, prob = .75, na.rm = TRUE), ndec),
                  count = n())
    } else if (method == "thirds") {
      es_values <- df %>%
        summarise(cdq16 = round(quantile(es, prob = .1665, na.rm = TRUE), ndec),
                  cdq50 = round(quantile(es, prob = .50, na.rm = TRUE), ndec),
                  cdq83 = round(quantile(es, prob = .8335, na.rm = TRUE), ndec),
                  count = n())
    } else {
      return(warning("Please enter a valid method"))
    }
    es_values <- as.data.frame(es_values)
    rownames(es_values) <- "Raw effect size"


  } else {
    if (method == "quads") {
      es_values <- df %>%
        # mutate({{ grouping_var }} := as.character({{ grouping_var }})) %>%
        # bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq25 = round(quantile(es, prob = .25, na.rm = TRUE), ndec),
                  cdq50 = round(quantile(es, prob = .50, na.rm = TRUE), ndec),
                  cdq75 = round(quantile(es, prob = .75, na.rm = TRUE), ndec),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise({{grouping_var}} := "All",
                                   cdq25 = round(quantile(es, prob = .25, na.rm = TRUE), ndec),
                                   cdq50 = round(quantile(es, prob = .50, na.rm = TRUE), ndec),
                                   cdq75 = round(quantile(es, prob = .75, na.rm = TRUE), ndec),
                                   count = n()))
    } else if (method == "thirds") {
      es_values <- df %>%
        # mutate({{grouping_var}} := as.character({{grouping_var}})) %>%
        # bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by({{ grouping_var }}) %>%
        summarise(cdq16 = round(quantile(es, prob = .1665, na.rm = TRUE), ndec),
                  cdq50 = round(quantile(es, prob = .50, na.rm = TRUE), ndec),
                  cdq83 = round(quantile(es, prob = .8335, na.rm = TRUE), ndec),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise({{grouping_var}} := "All",
                                   cdq16 = round(quantile(es, prob = .1665, na.rm = TRUE), ndec),
                                   cdq50 = round(quantile(es, prob = .50, na.rm = TRUE), ndec),
                                   cdq83 = round(quantile(es, prob = .8335, na.rm = TRUE), ndec),
                                   count = n()))
    } else {
      return("Please enter a valid method")
    }




    es_values <- es_values %>%
      filter(count >= min_group_size)
    es_values <- as.data.frame(es_values)
  }
  ifelse(method == "thirds",
    ifelse(missing(grouping_var),
         colnames(es_values) <- c("16.65%", "50%", "83.35%", "Number of effects"),
         colnames(es_values) <- c("Group", "16.65%", "50%", "83.35%", "Number of effects")),
    ifelse(missing(grouping_var),
           colnames(es_values) <- c("25%", "50%", "75%", "Number of effects"),
           colnames(es_values) <- c("Group", "25%", "50%", "75%", "Number of effects")))


  if (csv_write == TRUE) {
    write.csv(es_values, file = path_file_name)
  }
  return(es_values)
}
