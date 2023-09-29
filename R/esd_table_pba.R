#' Creating a table for field-specific effect size benchmarks, adjusted for
#' publication bias
#'
#' @param lim_obj An object of class "limitmeta".
#' @param grouping Defaults to FALSE. When set to TRUE, will detect subgrouping
#' of the lim_obj and calculate benchmarks for each group.
#' @param method Defaults to 'quads', can also be 'thirds'.
#' @param min_group_size Sets the minimum amount of effect sizes needed to
#' include a group in the table. Defaults to 3.
#' @param csv_write Defaults to FALSE. Will write the outputted table as a csv.
#' @param path_file_name A string containing the directory to which the .csv
#' file will be saved, including the title of the .csv file (has to end in
#' '.csv').
#' @param ndec The number of decimal places in which all values should be
#' reported. Defaults to 2.
#'
#' @return A table.
#' @export
#'
#' @examples
#' m2 <- meta::metagen(TE = ot_dat$yi,
#'                     seTE = ot_dat$sei,
#'                     subgroup = ot_dat$group)
#' l2 <- metasens::limitmeta(m2)
#' esd_table_pba(lim_obj = l2, grouping = TRUE)
#'
#'
esd_table_pba <- function(lim_obj,
                          grouping = FALSE,
                          method = "quads",
                          min_group_size = 3,
                          csv_write = FALSE,
                          path_file_name = "esd_table.csv",
                          ndec = 2) {
  stopifnot(class(lim_obj) == "limitmeta")
  if (isTRUE(grouping)) {
    df <- data.frame(lim_obj[1], # TE
                     lim_obj[3], # TE.limit
                     lim_obj[6], # TE.random
                     lim_obj[8], # lower.random
                     lim_obj[9], # upper.random
                     lim_obj[13], # TE.adjust
                     lim_obj[15], # lower.adjust
                     lim_obj[16], # upper.adjust
                     lim_obj[["x"]][["byvar"]])
    df$TE_abs <- abs(df$TE)
    df$TE.limit_abs <- abs(df$TE.limit)
    colnames(df)[9] <- "byvar"
    df <- as.data.frame(df)
  } else {
    df <- data.frame(lim_obj[1], # TE
                     lim_obj[3], # TE.limit
                     lim_obj[6], # TE.random
                     lim_obj[8], # lower.random
                     lim_obj[9], # upper.random
                     lim_obj[13], # TE.adjust
                     lim_obj[15], # lower.adjust
                     lim_obj[16]) # upper.adjust
    df$TE_abs <- abs(df$TE)
    df$TE.limit_abs <- abs(df$TE.limit)
    df <- as.data.frame(df)
  }



  if(isFALSE(grouping)) {
    if(method == "quads") {
      es_values <- df %>%
        summarise(cdq25 = round(quantile(TE_abs, prob = .25, na.rm = TRUE), ndec),
                  cdq25_adj = round(quantile(TE.limit_abs , prob = .25, na.rm = TRUE), ndec),
                  cdq50 = round(quantile(TE_abs, prob = .50, na.rm = TRUE), ndec),
                  cdq50_adj = round(quantile(TE.limit_abs, prob = .5, na.rm = TRUE), ndec),
                  cdq75 = round(quantile(TE_abs, prob = .75, na.rm = TRUE), ndec),
                  cdq75_adj = round(quantile(TE.limit_abs, prob = .75, na.rm = TRUE), ndec),
                  count = n())
    } else if (method == "thirds") {
      es_values <- df %>%
        summarise(cdq16 = round(quantile(TE_abs, prob = .1665, na.rm = TRUE), ndec),
                  cdq16_adj = round(quantile(TE.limit_abs, prob = .1665, na.rm = TRUE), ndec),
                  cdq50 = round(quantile(TE_abs, prob = .50, na.rm = TRUE), ndec),
                  cdq50_adj = round(quantile(TE.limit_abs, prob = .5, na.rm = TRUE), ndec),
                  cdq83 = round(quantile(TE_abs, prob = .8335, na.rm = TRUE), ndec),
                  cdq83_adj = round(quantile(TE.limit_abs, prob = .8335, na.rm = TRUE), ndec),
                  count = n())
    } else {
      return(warning("Please enter a valid method"))
    }
    es_values <- as.data.frame(es_values)
    adj_values <- data.frame(matrix("", nrow = 2, ncol = 4))
    rownames(adj_values) <- c("Raw effect size", "Adjusted effect size")

    for (i in 1:2) {
      for (j in 1:3) {
        index <- (i-1+(j*2-1))
        adj_values[i,j] <- format(round(as.numeric(es_values[index]), 2), nsmall = 2)
      }
      adj_values[i,4] <-as.numeric(es_values[7])
    }

  } else {
    if (method == "quads") {
      es_values <- df %>%
        # mutate({{ grouping_var }} := as.character({{ grouping_var }})) %>%
        # bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by(byvar) %>%
        summarise(cdq25 = round(quantile(TE_abs, prob = .25, na.rm = TRUE), ndec),
                  cdq25_adj = round(quantile(TE.limit_abs, prob = .25, na.rm = TRUE), ndec),
                  cdq50 = round(quantile(TE_abs, prob = .50, na.rm = TRUE), ndec),
                  cdq50_adj = round(quantile(TE.limit_abs, prob = .5, na.rm = TRUE), ndec),
                  cdq75 = round(quantile(TE_abs, prob = .75, na.rm = TRUE), ndec),
                  cdq75_adj = round(quantile(TE.limit_abs, prob = .75, na.rm = TRUE), ndec),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise(byvar := "All",
                                   cdq25 = round(quantile(TE_abs, prob = .25, na.rm = TRUE), ndec),
                                   cdq25_adj = round(quantile(TE.limit_abs, prob = .25, na.rm = TRUE), ndec),
                                   cdq50 = round(quantile(TE_abs, prob = .50, na.rm = TRUE), ndec),
                                   cdq50_adj = round(quantile(TE.limit_abs, prob = .5, na.rm = TRUE), ndec),
                                   cdq75 = round(quantile(TE_abs, prob = .75, na.rm = TRUE), ndec),
                                   cdq75_adj = round(quantile(TE.limit_abs, prob = .75, na.rm = TRUE), ndec),
                                   count = n()))
    } else if (method == "thirds") {
      es_values <- df %>%
        # mutate({{grouping_var}} := as.character({{grouping_var}})) %>%
        # bind_rows(mutate(., {{grouping_var}} := "All")) %>%
        group_by(byvar) %>%
        summarise(cdq16 = round(quantile(TE_abs, prob = .1665, na.rm = TRUE), ndec),
                  cdq16_adj = round(quantile(TE.limit_abs, prob = .1665, na.rm = TRUE), ndec),
                  cdq50 = round(quantile(TE_abs, prob = .50, na.rm = TRUE), ndec),
                  cdq50_adj = round(quantile(TE.limit_abs, prob = .5, na.rm = TRUE), ndec),
                  cdq83 = round(quantile(TE_abs, prob = .8335, na.rm = TRUE), ndec),
                  cdq83_adj = round(quantile(TE.limit_abs, prob = .8335, na.rm = TRUE), ndec),
                  count = n()) %>%
        ungroup() %>%
        bind_rows(df %>% summarise(byvar := "All",
                                   cdq16 = round(quantile(TE_abs, prob = .1665, na.rm = TRUE), ndec),
                                   cdq16_adj = round(quantile(TE.limit_abs, prob = .1665, na.rm = TRUE), ndec),
                                   cdq50 = round(quantile(TE_abs, prob = .50, na.rm = TRUE), ndec),
                                   cdq50_adj = round(quantile(TE.limit_abs, prob = .5, na.rm = TRUE), ndec),
                                   cdq83 = round(quantile(TE_abs, prob = .8335, na.rm = TRUE), ndec),
                                   cdq83_adj = round(quantile(TE.limit_abs, prob = .8335, na.rm = TRUE), ndec),
                                   count = n()))

    } else {
      return("Please enter a valid method")
    }

    es_values <- es_values %>%
      filter(count >= min_group_size)
    es_values <- as.data.frame(es_values)
    adj_values <- data.frame(matrix(nrow = (2 * nrow(es_values)), ncol = 4))
    adj_table_rownames <- c()

    for (h in 1:nrow(es_values)) {
      adj_table_rownames[h*2-1] <- as.character(es_values[h,1])
      adj_table_rownames[h*2] <- as.character(paste(es_values[h,1], "adjusted", sep = " "))
      for (i in 1:2) {
        row_index <- i-1+(h*2-1)
        for (j in 1:3) {
          col_index <- (i+(j*2)-1)
          adj_values[row_index,j] <- format(round(as.numeric(es_values[h,col_index]), 2), nsmall = 2)
        }
        adj_values[row_index,4] <- es_values[h,8]
      }
    }
    rownames(adj_values) <- adj_table_rownames

  }
  ifelse(method == "thirds",
         colnames(adj_values) <- c("16.65%", "50%", "83.35%", "Number of effects"),
         colnames(adj_values) <- c("25%", "50%", "75%", "Number of effects"))


  if (csv_write == TRUE) {
    write.csv(adj_values, file = path_file_name)
  }
  return(adj_values)
}
