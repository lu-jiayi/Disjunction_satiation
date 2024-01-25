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
raw_data_path <- "disjunction_exp1-merged.csv"
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
write.csv(d,"exp1_cleaned.csv", row.names = FALSE)


#############################
#############################
# plots and graphs
#############################

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")
d <- read.csv("exp1_cleaned.csv")
d$block <- d$trial_sequence_total - (d$trial_sequence_total %% 11)
zd <- d %>%
  group_by(workerid) %>%
  mutate(mean_response = mean(response),
         sd_response = sd(response))

# Z-score transform the response values by workerid

nd = d %>%
  mutate(condition_alt = fct_recode(condition_alt,"Clear Alternative"="alt","No Alternative"="no_alt", "Grammatical Disjunction"="gram", "Good Fillers"="filler_good", "Bad Fillers"="filler_bad"))%>%
 mutate(condition_cop = fct_recode(condition_cop,"Singular"="singular","Plural"="plural"))%>%
   group_by(workerid) %>%
 mutate(mean_response = mean(response),
     sd_response = sd(response))%>%
 mutate(zscore_response = (response - mean_response) / sd_response)



trial_means = nd %>%
  group_by(condition_alt,trial_sequence_total, condition_loc, condition_cop) %>%
  summarize(response = mean(response)) %>%
  ungroup()

cbPalette = c("#e69d00", "#009e74","#d55e00",  "#cc79a7", "#0071b2")

ggplot(nd, aes(x=trial_sequence_total, y=response, color = condition_alt, fill=condition_alt)) +
  geom_point(data=trial_means,alpha=.9) +
  xlab("trial sequence") +
  ylab("average acceptability")+
  geom_smooth(method=loess) +
  scale_color_manual(name="Condition", values=cbPalette) +
  scale_fill_manual(name="Condition", values=cbPalette) +
  theme_bw()

filtered_trial_means <- trial_means %>%
  filter(condition_alt != "Good Fillers" & condition_alt != "Bad Fillers")%>%
  mutate(condition_loc = coalesce(na_if(condition_loc, ""), "congruent"))

notrial_filtered_means <- filtered_trial_means %>%
  group_by(condition_alt, condition_loc, condition_cop)%>%
  summarise(mean_response = mean(response))
ggplot(filtered_trial_means, aes(x = condition_alt, y = response, fill = condition_loc)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cbPalette) +
  labs(x = "Condition: Alternative Availability",
       y = "Mean Response",
       fill = "Condition: Local Congruency") +
  theme_minimal()+facet_wrap(filtered_trial_means$condition_cop)

#only critical conditions, grouped by local congruency

d_alt <- subset(nd, condition_alt %in% c("Clear Alternative", "No Alternative"))
d_alt$condition_alt <- droplevels(d_alt$condition_alt)
levels(d_alt$condition_alt) <- c("Clear Alternative", "No Alternative")
d_alt$condition_cop <- as.factor(d_alt$condition_cop)
d_alt$condition_alt <- as.factor(d_alt$condition_alt)
d_alt$condition_cop <- droplevels(d_alt$condition_cop)
levels(d_alt$condition_cop)

d_alt = d_alt %>%
  mutate(condition_cop = fct_relevel(condition_cop, c("Singular", "Plural")))
trial_means_alt = d_alt %>%
  group_by(condition_alt, condition_loc, condition_cop, workerid,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup()
ggplot(d_alt, aes(x=trial_sequence_total, y=response, color = condition_loc, fill=condition_loc, linetype = condition_cop )) +
 # geom_point(data=trial_means_alt,alpha=.9) +
  xlab("trial sequence") +
  ylab("average acceptability")+
  geom_smooth(method = "lm", formula = y ~ log(x)) +
  scale_color_manual(name="Congruency", values=cbPalette) +
  scale_fill_manual(name="Congruency", values=cbPalette) +
  scale_linetype(name = "Copula Plurality")+
  theme_bw()+
  facet_wrap(d_alt$condition_alt)
  
  
  
  ###########Clustering analysis ##############
library(cluster)

trial_means_c <- nd %>%
  filter(condition_alt %in% c("Grammatical Disjunction", "Clear Alternative"))%>%
  
#  group_by(workerid) %>%
#  mutate(mean_response = mean(response),
#         sd_response = sd(response))%>%
#  mutate(zscore_response = (response - mean_response) / sd_response)%>%

  group_by(condition_alt, condition_cop, workerid) %>%
  #summarize(zscore_response = mean(zscore_response))%>% 
  summarize(response = mean(response))%>%

  ungroup()

# Filter the data for condition_alt = "Clear Alternative"
filtered_data <- trial_means_c %>% 
  filter(condition_alt %in% c("Grammatical Disjunction", "Clear Alternative"))

# Pivot the data to create a vector for each workerid
pivot_data <- filtered_data %>%
 #pivot_wider(names_from = c(condition_cop, condition_alt), values_from = zscore_response, values_fill = NA)
pivot_wider(names_from = c(condition_cop, condition_alt), values_from = response, values_fill = NA)


# Extract workerid and response vectors
response_vectors <- pivot_data[, -1]
workerid <- pivot_data$workerid

wss <- sapply(1:10, function(k) {
  kmeans_result <- kmeans(response_vectors, centers = k)
  kmeans_result$tot.withinss
})

# Plot the elbow curve
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters",
     ylab = "Total Within-cluster Sum of Squares")




# Perform k-means clustering
kmeans_result <- kmeans(response_vectors, centers = 3)

# Get cluster labels for each workerid
cluster_labels <- as.factor(kmeans_result$cluster)

# Combine cluster labels with workerid
clustered_workerid <- data.frame(workerid, cluster_labels)

# Print the clustered workerid
print(clustered_workerid)

#calculate distance scores 
distances <- apply(response_vectors, 1, function(x) sqrt(rowSums((x - kmeans_result$centers)^2)))
# Option 1: Calculate a score as the inverse of the distance (smaller distance = higher score)
#likelihood_scores <- 1 / distances

# Option 2: Use distance as score (greater distance = higher score)
likelihood_scores <- distances

# Transpose the likelihood_scores matrix to get scores for each workerid
worker_scores <- t(likelihood_scores)

# Create a data frame with workerid and the three scores
worker_scores_df <- data.frame(workerid, worker_scores)

# Print the worker scores
print(worker_scores_df)

merged_data <- filtered_data %>%
  left_join(clustered_workerid, by = "workerid")%>%
  left_join(worker_scores_df, by = "workerid")

averaged_responses <- merged_data %>%
  group_by(condition_alt, cluster_labels, condition_cop) %>%
  #summarize(averaged_response = mean(zscore_response))
  summarize(averaged_response = mean(response))

# Plot the clustering result as a bar plot
ggplot(averaged_responses, aes(x = condition_cop, y = averaged_response, fill = condition_alt)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cbPalette) +
  labs(x = "Condition: Alternative Availability",
       y = "Mean Response",
       fill = "Condition: copula plurality") +
  facet_wrap(~ cluster_labels ) +
  theme_minimal()



 trial_means_w = nd %>%
  left_join(clustered_workerid, by = "workerid")%>%
    group_by(condition_alt, condition_loc, condition_cop, cluster_labels) %>%
  summarize(response = mean(response)) %>%
  ungroup()
filtered_trial_means <- trial_means_w %>%
   filter(condition_alt != "Good Fillers" & condition_alt != "Bad Fillers")%>%
  mutate(condition_loc = coalesce(na_if(condition_loc, ""), "congruent"))

ggplot(filtered_trial_means, aes(x = condition_alt, y = response, fill = condition_loc)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cbPalette) +
  labs(x = "Condition: Alternative Availability",
       y = "Mean Response",
       fill = "Condition: Local Congruency") +
  theme_minimal()+facet_wrap(~ cluster_labels +condition_cop, ncol = 2 )+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))


# Calculate the counts of workerid in each cluster
cluster_counts <- table(clustered_workerid$cluster_labels)

# Print the cluster counts
print(cluster_counts)

#read in subject info

sub_info <- read.csv("subject.csv")

sub_info <- sub_info %>%
  left_join(clustered_workerid, by = "workerid")%>%
  left_join(worker_scores_df, by = "workerid")


sub_info <- sub_info[complete.cases(sub_info$age, sub_info$enjoyment, sub_info$education, sub_info$gender, sub_info$cluster_labels), ]
sub_info <- subset(sub_info, age < 200)
#library(languageR)
#pairscor.fnc(sub_info[,c( "X1","X2", "age")])


cluster_summary <- sub_info %>%
  group_by(cluster_labels) %>%
  summarize(male_count = sum(gender == "Male"),
            female_count = sum(gender == "Female"),
            other_count = sum(gender %notin% c("Male", "Female")),
              avg_age = mean(age),
            enjoyment = mean(enjoyment),
            education = mean(education))

# Print the cluster summary
print(cluster_summary)


#demographic and distance score correlation

sub_info <- sub_info %>%
  filter(gender %in% c("Female", "Male")) %>%
  mutate(gender = as.factor(gender),
         cluster_labels = as.factor(cluster_labels),
         cluster_labels = relevel(cluster_labels,2))

model1 <- lm(X1 ~  age* education+ gender + enjoyment  , data = sub_info)
summary(model1)

model2 <- lm(X2 ~ age * education+ gender + enjoyment   , data = sub_info)
summary(model2)

model3 <- lm(X3 ~ age * education + gender + enjoyment , data = sub_info)
summary(model3)

model_age <- glm(age~cluster_labels, data = sub_info)
summary(model_age)

model_education <- glm(education~cluster_labels, data = sub_info)
summary(model_education)






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


#############################
#############################
# Analysis
#############################
d_alt$trial_sequence_total <- scale(d_alt$trial_sequence_total, center = TRUE, scale = FALSE)

d_alt <- d_alt %>%
  mutate(condition_alt = as.numeric(d_alt$condition_alt) - mean(as.numeric(d_alt$condition_alt)))

d_alt <- d_alt %>%
  mutate(condition_cop = (as.numeric(d_alt$condition_cop) - mean(as.numeric(d_alt$condition_cop))))




model_alt <- lmer(response~trial_sequence_total*condition_alt*condition_cop + 
                                              (1+condition_alt*condition_cop|workerid)+
                                              (1+condition_alt*condition_cop|item_number), 
                                            data = d_alt)



#############Trying a log model
d_test <- subset(d_alt, condition_alt == "Clear Alternative")

log_model_log <- nls(response ~ log(trial_sequence_total, base = b), data = d_test, start = list(b = 10))
summary(log_model_log)
model_alt_log <- lm(response~log(trial_sequence_total), 
                      data = d_test)

model_alt_lin <- lm(response~trial_sequence_total, 
                      data = d_test)
summary(model_alt_lin)
summary(model_alt_log)
AIC(model_alt_log)
AIC(model_alt_lin)
BIC(model_alt_log)
BIC(model_alt_lin)

log_model <- brm(response ~ log(trial_sequence_total), data = d_test, 
                 family = gaussian(),save_pars = save_pars(all = TRUE))

# Linear model
lin_model <- brm(response ~ trial_sequence_total, data = d_test, 
                 family = gaussian(),save_pars = save_pars(all = TRUE))

summary(log_model)
summary(lin_model)
bayes_factor(log_model, lin_model)


# Calculate the WAIC for each model
log_waic <- loo(log_model)
lin_waic <- loo(lin_model)

# Compare the models using the WAIC
loo_compare(log_waic, lin_waic)
