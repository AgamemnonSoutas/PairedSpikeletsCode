rm(list = ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(forcats)

raw_data <- read_excel("raw_data.xlsx",sheet = 1)

raw_data$dev_stage <- as.factor(raw_data$dev_stage)
raw_data$dev_stage <- fct_relevel(raw_data$dev_stage,"TS","WA","GA","PM")


change_rate <- raw_data %>%
   group_by(variety,dev_stage) %>%
   summarise(florets_mean = mean(florets_spikelet_min_1),
             florets_sd = sd(florets_spikelet_min_1),
             florets_se = sd(florets_spikelet_min_1)/sqrt(n())) %>%
   mutate(change_percent = 100*(florets_mean - lag(florets_mean))/lag(florets_mean))

change_rate
change_rate %>%
   ggplot(.,aes(x=dev_stage,y=florets_mean,group=variety))+
   geom_point(aes(shape=variety))+
   geom_errorbar(aes(ymin=florets_mean-florets_se,
                     ymax=florets_mean+florets_se),width=.1)+
   geom_line(aes(linetype=variety))+cowplot::theme_cowplot()
anova1290<-aov(change_rate$florets_mean~change_rate$variety)
summary(anova1290)
