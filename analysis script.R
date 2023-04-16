# Loading libraries ---------------------------------
install.packages("dplyr", "ggplot2")
library(dplyr)
library(devtools)
library(ggplot2)


# Demonstrating package ---------------------------------

# Load all the functions from the R package
# load_all()

library(devtools)
devtools::install_github("berntgl/ESDist")
library(ESDist)

# Loading and filtering data =================================

# The ESDist package includes two data files; ot_dat_raw, which contains all
# effect sizes from the included meta-analyses, and ot_dat, which is the
# filtered version of that data. The code below demonstrates how the data can
# be filtered using the ci_to_se() helper function.

#load data
ot_dat_raw <- ot_dat_raw

# We create a new dataset called ot_dat, which we will filter.
ot_dat <- ot_dat_raw

# We will convert some effect sizes, so we create a new column with effect
# sizes as they were reported.
ot_dat$raw_es <- ot_dat$yi

# We also give every single effect size an ID, so we know which ones are
# eventually filtered out.
ot_dat$ID <- seq.int(nrow(ot_dat))

# We calculate Standard error for each effect size from the 95% CI.
ot_dat$sei[is.na(ot_dat$sei)] <- ci_to_se(ot_dat$lower[is.na(ot_dat$sei)], ot_dat$upper[is.na(ot_dat$sei)])

# Next, we convert all effect sizes we can to Cohen's d, based on group sizes.
# We use the des() function from the compute.es package.
library(compute.es)
ot_dat$yi <- des(d=raw_es, n.1=n1, n.2=n2, id=ID, data=ot_dat)[,13]

# Because we converted some effect sizes that were already reported as Hedges'
# g, we will overwrite those effect sizes with the raw effect size.
ot_dat$yi[ot_dat$es_type == "Hedges' g"] <- ot_dat$raw_es[ot_dat$es_type == "Hedges' g"]

# We will also include effect sizes reported in Cohen's d that have more than
# 20 participants, as these effect size are somewhat comparable to Hedges' g.
ot_dat$yi[ot_dat$n_total >= 20 & is.na(ot_dat$yi)] <- ot_dat$raw_es[ot_dat$n_total >= 20 & is.na(ot_dat$yi)]

# Filter out the effect sizes with lowest SE per group per study (some studies
# have multiple groups), and the effects with the lowest SE. In case some
# effects from the same study have the same SE, we only use the effect size
# that is closest to zero in absolute terms.
ot_dat <- ot_dat %>%
  group_by(study_doi, group) %>%
  filter(!is.na(yi)) %>%
  filter(sei == min(sei)) %>%
  filter(abs(yi) == min(abs(yi))) %>%
  ungroup()

# Finally, we add a column with only absolute effect sizes, called yi_abs.
ot_dat$yi_abs <- abs(ot_dat$yi)

# The filtering process messes up the data type a bit, so let's turn it back
# into a dataframe
ot_dat <- as.data.frame(ot_dat)


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

abs_mean <- mean(ot_dat$yi_abs)
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
# the direction. Below, we first create a column in our dataset called yi_abs,
# which contains only absolute effect sizes. We then save a plot using the
# thirds method to a variable called plot3, and a plot using the quads method
# to a variable called plot4. We will also calculate the corresponding values
# for each percentile


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
                  sesoi = 0.2)

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
                  sesoi = 0.2)

plot6


# esd_plot_group() =================================

# Our dataset has several subgroups (e.g., healthy participants, ASD,
# Schizophrenia, etc.) that we might want to compare. In the code below we
# compare plots for each group and save the figure to a variable called plot7.
# Although, theoretically,we could compare all groups, some groups only have
# one or a few studies. As such, these groups are not very informative. The
# esd_plot_group() function therefore only includes groups with at least 20
# studies

plot7 <- esd_plot_group(df = ot_dat,
                    es = yi,
                    es_type = "Hedges' g",
                    grouping_var = group)
plot7

# We will now calculate and plot a vertical line for the mean of each group and
# save the figure to a variable called plot8.

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

# In case we want to include groups with fewer studies, we can also specify
# min_group_size to a value smaller than its default (20). The code below is
# exactly the same as for plot9, except that all groups with at least 15
# studies are included.
plot9b <- esd_plot_group(df = ot_dat,
                        es = yi_abs,
                        es_type = "Hedges' g",
                        grouping_var = group,
                        method = 'quads',
                        min_group_size = 15)

plot9b


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
# calculation. We save the result to a variable called table1a.
# This function defaults to the quads method, which means that it calculates
# the 25th, 50th, and 75th percentiles to correspond to small, medium, and
# large effect sizes respectively. It is also possible to use the thirds method
# by adding method = 'thirds'. In this case, we calculate the 16.65th, 50th, and
# 83.35th percentiles. We save the result to a variable called table1b.

table1a <- esd_table(df = ot_dat,
                     es = yi_abs)

table1a

table1b <- esd_table(df = ot_dat,
                    es = yi_abs,
                    method = "thirds")

table1b



# In case we want to compare benchmarks between groups, we can define our
# grouping variable in the function as well. In this case, we also get a summary
# of all effect sizes in the bottom row. We save the results to a variable
# called table2a (or table2b for the thirds method).

table2a <- esd_table(df = ot_dat,
                     es = yi_abs,
                     grouping_var = group)

table2a


table2b <- esd_table(df = ot_dat,
                     es = yi_abs,
                     grouping_var = group,
                     method = "thirds")

table2b


# In some cases, we might want to compare between different variables, like
# study design. Here we compare studies with within- and between-subject
# designs and store the results in a variable called table3a (and table3b for
# the thirds approach).

table3a <- esd_table(df = ot_dat,
                    es = yi_abs,
                    grouping_var = design)

table3a

table3b <- esd_table(df = ot_dat,
                     es = yi_abs,
                     grouping_var = design,
                     method = "thirds")

table3b

# We can save this table as a .csv file by adding another argument to our
# function. By setting csv_write = TRUE, and by specifying a file_name argument,
# we save our table as a .csv file to the directory we specify in our R
# environment. Below, we copied the definition of table3, but added the command
# to save the table as a .csv file.

table3 <- esd_table(df = dat_filt_abs,
                    es = yi,
                    grouping_var = group,
                    csv_write = TRUE,
                    file_name = "table3.csv")


