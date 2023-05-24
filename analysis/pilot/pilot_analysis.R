this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(simr)
library(lmerTest)
library(brms)
library(bootstrap)
library(ggpubr)

`%notin%` <- Negate(`%in%`)
raw_data_path <- "pilot_raw.csv"
data<-read.csv(raw_data_path)

#############################
# Step 1: Filter out the participants who responded incorrectly more than once to the practice questions:
#############################

excluded_subjects <- c()
practice_data=subset(data,item_type %in% c("practice_bad", "practice_good"))
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(data, !is.element(workerid, practice_good_data$workerid))$workerid)
data=subset(data, is.element(workerid, practice_good_data$workerid))

length(unique(data$workerid))


#############################
# Step 2: filter: no overlap of 95%CI of FILL and UNGRAM
#############################

filler_data = subset(data, condition_gram == "filler_good")
ungram_data = subset(data, condition_gram == "filler_bad")

theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

filler_by_subject = aggregate(filler_data[,"response"],list(filler_data$workerid), ci.low)
ungram_by_subject = aggregate(ungram_data[,"response"],list(ungram_data$workerid), ci.high)

names(filler_by_subject)[names(filler_by_subject) == "Group.1"] <- "subject"
names(filler_by_subject)[names(filler_by_subject) == "x"] <- "fill_ci_low"

names(ungram_by_subject)[names(ungram_by_subject) == "Group.1"] <- "subject"
names(ungram_by_subject)[names(ungram_by_subject) == "x"] <- "ungram_ci_high"

all_filler <- merge(ungram_by_subject, filler_by_subject, by.x="subject")

eligible_subjects = c()
for (i in (1:length(all_filler$subject))){
  row = all_filler[i,]
  if (row$ungram_ci_high < row$fill_ci_low){
    eligible_subjects <- c(eligible_subjects, row$subject)
  }
  else{
    excluded_subjects <- c(excluded_subjects, row$subject)
  }
}
data = subset(data, workerid %in% eligible_subjects)

length(unique(data$workerid))

#############################
# save cleaned data
#############################

#Clean practice trials and control trials.
data = subset(data, item_type %notin% c("practice_bad", "practice_good"))
data$condition_loc <- factor(data$condition_loc, levels = c("congruent", "incongruent"))
data <- data %>%
  mutate(condition_alt = ifelse(condition_alt == "", condition_gram, condition_alt))

d=transform(data, trial_sequence_total = as.numeric(trial_sequence_total))
write.csv(d,"pilot_cleaned.csv", row.names = FALSE)


#############################
#############################
# plots and graphs
#############################

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")
d <- read.csv("pilot_cleaned.csv")

nd = d %>%
  mutate(condition_alt = fct_recode(condition_alt,"Clear Alternative"="alt","No Alternative"="no_alt", "Grammatical Disjunction"="gram", "Good Fillers"="filler_good", "Bad Fillers"="filler_bad"))
 
trial_means = nd %>%
  group_by(condition_alt,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()

cbPalette = c("#e69d00", "#009e74","#d55e00",  "#cc79a7", "#0071b2")

ggplot(nd, aes(x=trial_sequence_total, y=response, color = condition_alt, fill=condition_alt)) +
  geom_point(data=trial_means,alpha=.9) +
  xlab("trial sequence") +
  ylab("average acceptability")+
  geom_smooth(method=lm) +
  scale_color_manual(name="Condition", values=cbPalette) +
  scale_fill_manual(name="Condition", values=cbPalette) +
  theme_bw()

#only critical conditions, grouped by local congruency

d_alt <- subset(nd, condition_alt %in% c("Clear Alternative", "No Alternative"))
trial_means_alt = d_alt %>%
  group_by(condition_alt, condition_loc, workerid,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()
ggplot(d_alt, aes(x=trial_sequence_total, y=response, color = condition_alt, fill=condition_alt, linetype = condition_loc )) +
  geom_point(data=trial_means_alt,alpha=.9) +
  xlab("trial sequence") +
  ylab("average acceptability")+
  geom_smooth(method=lm) +
  scale_color_manual(name="Condition", values=cbPalette) +
  scale_fill_manual(name="Condition", values=cbPalette) +
  scale_linetype(name = "Local Congruency")+
  theme_bw()


#only critical conditions, grouped by copula plurality
d_cop <- subset(nd, condition_alt %in% c("Clear Alternative", "No Alternative"))
trial_means_cop = d_cop %>%
  group_by(condition_alt, condition_cop, trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()
ggplot(d_cop, aes(x=trial_sequence_total, y=response, color = condition_alt, fill=condition_alt, linetype = condition_cop)) +
  geom_point(data=trial_means_cop,alpha=.9) +
  xlab("trial sequence") +
  ylab("average acceptability")+
  geom_smooth(method=lm) +
  scale_color_manual(name="Condition", values=cbPalette) +
  scale_fill_manual(name="Condition", values=cbPalette) +
  scale_linetype(name = "Copula Plurality")+
  theme_bw()
