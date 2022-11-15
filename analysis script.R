library(dplyr)
library(devtools)
library(ggplot2)

setwd("../fxsizedistr")

dat <- read.csv("dat.csv")

adj_es_test <- dat %>%
  group_by(group) %>%
  summarise(median = median(yi))

adj_es_test <- as.vector(c(adj_es_test$median, median(dat$yi)))
adj_es_test <- adj_es_test - 0.05
adj_es_test

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

esd_plot(dat, yi, "cohen's d", method = "thirds")
esd_table(dat, yi)
pb_adj(dat, yi, 0.2)
esd_table_pba(dat, yi, adj_es_test, grouping_var=group)

list <- c()
for (i in 1:5) {
  list[i] <- vangogh_palette("SelfPortrait")[i]
}


