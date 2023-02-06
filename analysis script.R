library(dplyr)
library(devtools)
library(ggplot2)
library(gridExtra)

setwd("../ESDist")

#load data
dat <- read.csv("dat.csv")

# Filter out the effect sizes with lowest SE per group per study (some studies
# have multiple groups),
dat_filt <- dat %>%
  group_by(study_doi, group) %>%
  filter(sei == min(sei)) %>%
  filter(abs(yi) == min(abs(yi))) %>%
  ungroup()

# The filtering process messes up the data type a bit, so let's turn it back
# into a dataframe
dat_filt <- as.data.frame(dat_filt)

# We want to 'flip' the effect sizes of the studies where a negative ES
# is actually in favour of oxytocin. For example when they measure a reduction
# in symptoms
dat_filt$yi[dat_filt$favours_oxytocin == "negative"] <- dat_filt$yi[dat_filt$favours_oxytocin == "negative"] * -1

# For the effect size distribution, we're primarily interested in the size of
# the effects, but not necessarily the direction. If someone finds an effect
# of d = -0.5, then we we know that there is a possibility that we'll find an
# effect of d = 0.5 in either direction.
dat_filt_abs <- dat_filt
dat_filt_abs$yi <- abs(dat_filt_abs$yi)
dat_filt_abs <- as.data.frame(dat_filt_abs)


dat_corr <- dat
dat_corr$yi[dat_corr$favours_oxytocin == "negative"] <- dat_corr$yi[dat_corr$favours_oxytocin == "negative"] * -1

dat_abs <- dat_corr
dat_abs$yi <- abs(dat_abs$yi)
dat_abs <- as.data.frame(dat_abs)


###

# Load all the functions from the R package
load_all()

# Plot the ESD.
plot1 <- esd_plot(dat_filt_abs, yi, "Cohen's d", method = "quads", pop_es = 0.2, mean = TRUE)
plot1
plot2 <- esd_plot_perc(dat_filt_abs, yi, "Cohen's d")
plot2


# Get the values for each benchmark
esd_table(dat_filt_abs, yi)



# Creating an artificial vector of "adjusted" effect sizes for each group.
adj_es_test_group <- dat_filt_abs %>%
  group_by(group) %>%
  summarise(mean = mean(yi))

adj_es_test_group <- as.vector(c(adj_es_test_group$mean, mean(dat_filt_abs$yi)))
adj_es_test_group <- adj_es_test_group - 0.05
adj_es_test_group

#And doing the same for every individual meta-analysis
adj_es_test_ma <- dat_abs %>%
  group_by(meta_analysis) %>%
  summarise(mean = mean(yi))

adj_es_test_ma <- as.vector(c(adj_es_test_ma$mean, mean(dat_abs$yi)))
adj_es_test_ma <- adj_es_test_ma - 0.05
adj_es_test_ma

# Looks good! Let's see what it does

esd_table(dat_filt_abs, yi, group)
esd_table_pba(dat_filt_abs, yi, adj_es_test_group, grouping_var=group)

esd_table(dat_abs, yi, grouping_var = meta_analysis)
esd_table_pba(dat_abs, yi, adj_es_test_ma, grouping_var=meta_analysis)


esd_table_pba(dat_filt, yi, 0.1)
