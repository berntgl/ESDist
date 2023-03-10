# Loading libraries ---------------------------------

library(dplyr)
library(devtools)
library(ggplot2)

# Loading and filtering data ---------------------------------

# Now we have two datasets. dat_filt will have all effect sizes where positive
# effects favour oxytocin and negative effects do not favour oxytocin.
# dat_filt_abs will have absolute values only.


# Demonstrating package ---------------------------------

# Load all the functions from the R package
# load_all()

library(devtools)
devtools::install_github("berntgl/ESDist")
library(ESDist)

ot_dat$yi_abs <- abs(ot_dat$yi)
# esd_plot() =================================


# First, we will Plot the effect size distribution (ESD) and save it to a
# variable called 'plot1'
plot1 <- esd_plot(df = ot_dat,
                  es = yi,
                  es_type = "Hedges' g")
plot1


# Next, we will also plot the mean of the distribution, which corresponds to
# d = 0.22. We'll save the plot to a variable called plot2

true_mean <- mean(ot_dat$yi)
true_mean

abs_mean <- mean(dat_filt_abs$yi)
abs_mean

plot2 <- esd_plot(df = ot_dat,
                  es = yi,
                  es_type = "Hedges' g",
                  mean = "mean")

plot2


# If we want to calculate small/medium/large effect size benchmarks, we can do
# so by specifying a method. We can choose between the more common "quads"
# method and the "thirds" method that was used by Schäfer and Schwarz (2019).
# When we want to calculate such benchmarks, we need to use the absolute effect
# sizes only, because we are interested in only the size of each effect, not
# the direction. Below, we save a plot using the thirds method to a variable
# called plot3, and a plot using the quads method to a variable called plot4.
# We will also calculate the corresponding values for each percentile

plot3 <- esd_plot(df = ot_dat, #we will now use absolute ES values only
                  es = yi_abs,
                  es_type = "Hedges' g",
                  method = "quads")

plot3

quantile(ot_dat$yi_abs, probs = .1665) # small, d = 0.033
quantile(ot_dat$yi_abs, probs = .5) # medium, d = 0.243
quantile(ot_dat$yi_abs, probs = .8335) # large, d = 0.754


plot4 <- esd_plot(df = ot_dat, #we will now use absolute ES values only
                  es = yi_abs,
                  es_type = "Hedges' g",
                  method = "thirds")

plot4

quantile(ot_dat$yi_abs, probs = .25) # small, d = 0.067
quantile(ot_dat$yi_abs, probs = .5) # medium, d = 0.243
quantile(ot_dat$yi_abs, probs = .75) # large, d = 0.550



# If we want to determine the range of effect sizes that a study design can
# detect, we can do so by setting an effect size of interest (esoi). In this
# case, we will pretend that we have a study that can detect an effect size of
# d = 0.2 with enough power. By adding the argument esoi = 0.2, we can
# calculate and visualise the range of empirical effect sizes that we can
# reliably detect. In the code below, we create such a plot and save it to a
# variable called plot5. The plot also tells us that 55.17% of the empirical
# effect sizes found in this field are larger than 0.2. This means that with
# our study design, we can detect 55.17% of empirical effect sizes.

plot5 <- esd_plot(df = ot_dat, #we will now use absolute ES values only
                  es = yi_abs,
                  es_type = "Hedges' g",
                  esoi = 0.2)

plot5


# Of course, it is also possible to combine the above features in a plot. In the
# code below, we combine all the features from above into a single plot and save
# it to a variable called plot6. In this plot, we can clearly see that our esoi
# falls between our benchmarks for a small and medium effect size. (Note that
# the mean is set to the true mean effect size, i.e., the mean of all non-
# absolute effect sizes.)

plot6 <- esd_plot(df = ot_dat, #we will now use absolute ES values only
                  es = yi_abs,
                  es_type = "Hedges' g",
                  method = "quads",
                  mean = true_mean,
                  esoi = 0.2)

plot6


# esd_plot_group() =================================

# Our dataset has several subgroups (e.g., healthy participants, ASD,
# Schizophrenia, etc.) that we might want to compare. Although, theoretically,
# we could compare all groups, some groups only have one or a few studies. As
# such, these groups are not very informative. In the code below, we first
# filter out the ASD, healthy, and SCZ groups and save it to a dataset called
# dat_groups. We then create a second dataset with only absolute effect
# sizes, called dat_groups_abs.


# Now that we have our datasets, we can start comparing groups. First we create
# a simple plot for each group. We save the figure to a variable called plot7.

plot7 <- esd_plot_group(df = ot_dat,
                    es = yi,
                    es_type = "Hedges' g",
                    grouping_var = group)
plot7

# We will now calculate and plot a vertical line for the mean of each group and save the
# figure to a variable called plot8.

plot8 <- esd_plot_group(df = ot_dat,
                        es = yi,
                        es_type = "Hedges' g",
                        grouping_var = group,
                        mean = 'mean')

plot8


# We can also use this type of visualisation to compare effect size benchmarks.
# We will use the absolute effect sizes for this and we will use the 'quads'
# approcah (note that it is also possible to use the thirds approach). We store
# the figure in a variable called plot9.

plot9 <- esd_plot_group(df = ot_dat,
                        es = yi_abs,
                        es_type = "Hedges' g",
                        grouping_var = group,
                        method = 'quads')

plot9


# Another interesting comparison within our dataset is between within-subjects
# and between-subjects study designs. Below, we create new datasets and plot
# a figure in which we can compare the two

plot10 <- esd_plot_group(df = ot_dat,
                        es = yi,
                        es_type = "Hedges' g",
                        grouping_var = design,
                        mean = "mean")
plot10

plot11 <- esd_plot_group(df = ot_dat,
                        es = yi_abs,
                        es_type = "Hedges' g",
                        grouping_var = design,
                        method = 'quads')

plot11


# esd_table() =================================

# If we want to just have an idea about the effect size benchmarks in our field
# of interest, we can use the esd_table() function. In order to use this
# function, we need to use the dataset with absolute values only. Below, we
# create a table that calculates the small, medium, and large effect size
# benchmarks and displays the number of effects that were used in the
# calculation. We save the result to a variable called table1.

table1a <- esd_table(df = ot_dat,
                     es = yi_abs)

table1a

table1b <- esd_table(df = ot_dat,
                    es = yi_abs,
                    method = "thirds")

table1b


# This function defaults to the quads method, which means that it calculates
# the 25th, 50th, and 75th percentiles to correspond to small, medium, and
# large effect sizes respectively. It is also possible to use the thirds method
# by adding method = 'thirds'. In this case, we calculate the 16.65th, 50th, and
# 83.35th percentiles. We save the result to a variable called table2.

table2 <- esd_table(df = dat_filt_abs,
                    es = yi,
                    method = 'thirds')

table2

# In case we want to compare benchmarks between groups, we can define our
# grouping variable in the function as well. In this case, we also get a summary
# of all effect sizes in the bottom row. We save the results to a variable
# called table3.

table2a <- esd_table(df = ot_dat,
                     es = yi_abs,
                     grouping_var = group)

table2a

table2b <- esd_table(df = ot_dat,
                     es = yi_abs,
                     grouping_var = group,
                     method = "thirds")

table2b


table3a <- esd_table(df = ot_dat,
                    es = yi_abs,
                    grouping_var = design)

table3a

table3b <- esd_table(df = ot_dat,
                     es = yi_abs,
                     grouping_var = design,
                     method = "thirds")

table3b

# If we want to save this table as a .csv file by adding another argument to our
# function. By setting csv_write = TRUE, we save our table as a .csv file called
# esd_table.csv to the directory we specify in our R environment. Below, we
# copied the definition of table3, but added the command to save the table as
# a .csv file.

table3 <- esd_table(df = dat_filt_abs,
                    es = yi,
                    grouping_var = group,
                    csv_write = TRUE)


# esd_table_pba() =================================

# Whenever we are dealing with empirical results, there is a severe possibility
# that there is bias in the data we are working with. One of the best ways to
# detect and adjust for this bias, is through Robust Meta-analysis. In the code
# below, we call the RoBMA package and use it on our data to determine the
# evidence for the presence of bias, as well as to calculate an adjusted
# average effect size.

library(RoBMA)

fit <- RoBMA(d = dat_filt$yi,
             se = dat_filt$sei,
             study_names = dat_filt$study,
             seed = 2333)
summary(fit)


# Based on these results, the adjusted effect size is 0.13. By using the
# esd_table_pba() function, we can estimate effect size benchmarks based on
# this adjusted effect size. It takes mostly the same arguments as the
# esd_table() function, with the addition of an adj_es argument, which
# corresponds to our adjusted effect size for this data. Below, we calculate
# the adjusted effect size benchmarks using the quads method and our adjusted
# effect size. We save the results to a variable called table 4.

table4 <- esd_table_pba(df = dat_filt_abs,
                        es = yi,
                        adj_es = 0.13)

table4


# By calculating the adjusted effect size for each group of interest (as well
# as our overall data), and then passing those adjusted effect sizes into a
# vector, we can also compare our adjusted benchmarks estimates between groups.
# Below, we use our dat_groups dataframe, calculate the adjusted benchmarks per
# group, and pass the results into a variable called table5.

adj_es_groups <- c(0.10, 0.12, 0.14, 0.13)

table5 <- esd_table_pba(df = dat_groups_abs,
                        es = yi,
                        adj_es = adj_es_groups,
                        grouping_var = group)
table5


ggsave(plot11,
       file = '../../Documents/Thesis/Figures/plot11.png',
       width = 10,
       height = 7,
       dpi = 300
)
