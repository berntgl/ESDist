library(dplyr)
library(devtools)
library(ggplot2)
library(gridExtra)

setwd("../fxsizedistr")

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

# We wan't to 'flip' the effect sizes of the studies where a negative ES
# is actually in favour of oxytocin. For example when they measure a reduction
# in symptoms
dat_filt$yi[dat_filt$favours_oxytocin == "negative"] <- dat_filt$yi[dat_filt$favours_oxytocin == "negative"] * -1

# For the effect size distribution, we're primarily interested in the size of
# the effects, but not necessarily the direction. If someone finds an effect
# of d = -0.5, then we we know that there is a possibility that we'll find an
# effect of d = 0.5 in either direction.
dat_filt_abs <- dat_filt
dat_filt_abs$yi <- abs(dat_filt_abs$yi)

###

# Load all the functions from the R package
load_all()

# Plot the ESD.
plot1 <- esd_plot(dat_filt_abs, yi, "Cohen's d", method = "thirds")
plot1
plot2 <- esd_plot_perc(dat_filt_abs, yi, "Cohen's d")
plot2


# Get the values for each benchmark
esd_table(dat_filt_abs, yi)



# Creating an artificial vector of "adjusted" effect sizes for each group.
adj_es_test <- dat_filt_abs %>%
  group_by(group) %>%
  summarise(mean = mean(yi))

adj_es_test <- as.vector(c(adj_es_test$mean, mean(dat$yi)))
adj_es_test <- adj_es_test - 0.05
adj_es_test

# Looks good! Let's see what it does

cdq16 <- quantile(dat$yi, prob = 0.1665, na.rm = TRUE)
cdq25 <- quantile(dat$yi, prob = 0.25, na.rm = TRUE)
cdq50 <- quantile(dat$yi, prob = 0.50, na.rm = TRUE)
cdq75 <- quantile(dat$yi, prob = 0.75, na.rm = TRUE)
cdq83 <- quantile(dat$yi, prob = 0.8335, na.rm = TRUE)


dat <- dat %>%
  mutate(rank = percent_rank(yi))

q <- ggplot(data = dat, aes(x = rank, y=yi, col=vangogh_palette("SelfPortrait")[1])) +
  geom_point() +
  geom_smooth(size = 1) +
  scale_color_manual(values = vangogh_palette("SelfPortrait")) +
  scale_fill_manual(values = vangogh_palette("SelfPortrait")) +
 #geom_hline(yintercept = mean(datnpr$r_total), linetype = "dashed", col = vangogh_palette("SelfPortrait")[1])+
  #geom_hline(yintercept = mean(datpr$r_total), linetype = "dashed", col = vangogh_palette("SelfPortrait")[2])+
  geom_vline(xintercept = .1665, linetype = "dashed", col = vangogh_palette("SelfPortrait")[5], size = 1)+
  geom_vline(xintercept = .25, linetype = "dashed", col = vangogh_palette("SelfPortrait")[4], size = 1)+
  geom_vline(xintercept = .5, linetype = "dashed", col = vangogh_palette("SelfPortrait")[3], size = 1)+
  geom_vline(xintercept = .75, linetype = "dashed", col = vangogh_palette("SelfPortrait")[4], size = 1)+
  geom_vline(xintercept = .8335, linetype = "dashed", col = vangogh_palette("SelfPortrait")[5], size = 1)

q

load_all()

esd_plot(dat_filt, yi, "cohen's d", method = "thirds")
esd_table(dat_filt, yi)
esd_table_pba(dat_filt, yi, adj_es_test, grouping_var=group)

q <- esd_plot(dat, yi, "Cohen's d", bin_width = 0.10)
q
q +
  geom_vline(xintercept = mean(dat$yi), linetype = "dashed", col = vangogh_palette("SelfPortrait")[3], size = 1)


esd_table_pba(dat_filt, yi, 0.1)


r <- ggplot(data = dat) +
  geom_histogram(aes(yi, fill = stat(x) > 0.3)) +
  scale_fill_manual(values = vangogh_palette("SelfPortrait"))
r
