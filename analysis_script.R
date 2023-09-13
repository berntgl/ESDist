# Loading libraries ---------------------------------
install.packages("dplyr", "ggplot2")
library(dplyr)
library(ggplot2)

# Loading package ---------------------------------

library(devtools)
devtools::install_github("berntgl/ESDist")
library(ESDist)


# Demonstrating package ---------------------------------


# esd_plot() =================================

# First, we will Plot the effect size distribution (ESD) and save it to a
# variable called 'plot1'
plot1 <- esd_plot(df = ot_dat,
                  es = yi,
                  es_type = "Hedges' g")
plot1


# If we want to calculate small/medium/large effect size benchmarks, we can do
# so by specifying a method. We can choose between the more common "quads"
# method and the "thirds" method that was used by Schäfer and Schwarz (2019).
# When we want to calculate such benchmarks, we need to use the absolute effect
# sizes only. Below, we first create a column in our dataset called yi_abs,
# which contains only absolute effect sizes. We then save a plot using the
# thirds method to a variable called plot2, and a plot using the quads method
# to a variable called plot4. We will also calculate the corresponding values
# for each percentile


plot2 <- esd_plot(df = ot_dat, #we will now use absolute ES values only
                  es = yi,
                  es_type = "Hedges' g",
                  method = "quads")

plot2


# If we want to determine the range of effect sizes that a study design can
# detect, we can do so by setting a smallest effect size of interest (sesoi). In this
# case, we will pretend that we have a study that can detect an effect size of
# d = 0.3 with enough power. By adding the argument sesoi = 0.3, we can
# calculate and visualise the range of empirical effect sizes that we can
# reliably detect. In the code below, we create such a plot and save it to a
# variable called plot3. The plot also tells us that 55.17% of the empirical
# effect sizes found in this field are larger than 0.3. This means that with
# our study design, we can detect 55.17% of empirical effect sizes.

plot3 <- esd_plot(df = ot_dat, #we will now use absolute ES values only
                  es = yi_abs,
                  es_type = "Hedges' g",
                  sesoi = 0.3)

plot3


# esd_plot_group() =================================

# Our dataset has several subgroups (e.g., healthy participants, ASD,
# Schizophrenia, etc.) that we might want to compare. In the code below we
# compare plots for each group and save the figure to a variable called plot4.
# Although, theoretically,we could compare all groups, some groups only have
# one or a few studies. As such, these groups are not very informative. The
# esd_plot_group() function therefore only includes groups with at least 20
# studies

plot4 <- esd_plot_group(df = ot_dat,
                    es = yi,
                    es_type = "Hedges' g",
                    grouping_var = group)
plot4

# We can also use this type of visualisation to compare effect size benchmarks.
# We will use the absolute effect sizes for this and we will use the 'quads'
# approcah (note that it is also possible to use the thirds approach). We store
# the figure in a variable called plot5.

plot5 <- esd_plot_group(df = ot_dat2,
                        es = yi,
                        es_type = "Hedges' g",
                        grouping_var = group,
                        method = 'quads')

plot5

# esd_plot_pba() =================================

# Unfortunately, a lot of published empirical results are prone to biases, such
# as publication bias. The metasens package (Schwarzer et al., 2023) uses
# limit meta-analysis to adjust individual effect sizes for publication bias.
# using the esd_plot_pba() function, we can plot the distribution of adjusted
# effect sizes against the distribution of unadjusted effect sizes. First, we
# create a meta-object (m1), based on the effect sizes and corresponding
# standard error in our dataset. Next, we create an object of class 'limitmeta',
# (l1) which we can then use in our esd_plot_pba() function. The resulting plot
# automatically generates a visualisation for the summary effect size and the
# corresponding 95% CI.

install.packages("meta", "metasens")
library(meta)
library(metasens)

m1 <- metagen(TE = ot_dat$yi, seTE = ot_dat$sei)
l1 <- limitmeta(m1)

plot6 <- esd_plot_pba(lim_obj = l1, es_type = "Hedges' g")
plot6

# We can also visualise the adjusted effect size benchmarks for our
# distribution, by setting the method argument. We save the result to plot7.

plot7 <- esd_plot_pba(lim_obj = l1,
                       es_type = "Hedges' g",
                       method = "quads")
plot7

# Finally, we can visualise the range of detectable ESs based on a sesoi.

plot7 <- esd_plot_pba(lim_obj = l1,
                      es_type = "Hedges' g",
                      sesoi = 0.3)
plot7

# esd_table() =================================

# The esd_table() function allows you to calculate effect size benchmarks. In
# its simplest form, it takes a dataframe and the name of the column housing
# all absolute effect sizes.

table1 <- esd_table(df = ot_dat,
                     es = yi)

table1



# In case we want to compare benchmarks between groups, we can define our
# grouping variable in the function as well. In this case, we also get a summary
# of all effect sizes in the bottom row. We save the results to a variable
# called table2a.

table2 <- esd_table(df = ot_dat,
                     es = yi,
                     grouping_var = group)

table2


# We can save this table as a .csv file by adding another argument to our
# function. By setting csv_write = TRUE, and by specifying a file_name argument,
# we save our table as a .csv file to the directory we specify in our R
# environment. Below, we copied the definition of table3, but added the command
# to save the table as a .csv file.

esd_table(df = ot_dat,
          es = yi,
          grouping_var = group,
          csv_write = TRUE,
          file_name = "table3.csv")

# esd_table_pba() =================================

# We can also calculate benchmarks for the publication bias-adjusted ESD. Like
# the plotting function, the esd_table_pba() function takes a 'limitmeta'
# object. Here, we use the same l1 object we used earlier.

table3 <- esd_table_pba(lim_obj = l1)

table3

# In case you want to create benchmarks (and adjusted benchmarks) per group,
# you will have to define a subgroup argument when creating the meta-object.
# Here we create a new 'meta' object and corresponding 'limitmeta' object (m2
# and l2) where we group the data based on the 'group' column in our dataset.

m2 <- metagen(TE = ot_dat$yi, seTE = ot_dat$sei, subgroup = ot_dat$group)
l2 <- limitmeta(m2)


# We can then use the l2 object in our table function, where we set the grouping
# argument to TRUE.

table4 <- esd_table_pba(lim_obj = l2,
                        grouping = TRUE)

table4


# esd_perc() =================================

# If we want to very simply calculate the percentile of a specific value, we
# can use the esd_perc() function. In the code below we calculate the percentile
# corresponding to g = 0.2. Note that we use yi_abs, as we are only interested
# in the size of all effects, not in their direction.

esd_perc(df = ot_dat,
         es = yi,
         value = 0.2)



