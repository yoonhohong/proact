# predict the slope of ALSFRS total scores 
# features: Age, Gender, onset_site, onset_delta, ALSFRS total score, FVC  
# algorithm: lasso linear regression

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT") # dropbox folder to import datasets 

library(dplyr)
library(ggplot2)
library(mice) # multiple imputation 
library(VIM) # visualization of missing data
library(caret) # machine learning 
library(ModelMetrics) 
library(gridExtra)


# Import ALSFRS_slope dataset (target variable, 3-12 mo slope)  
slope = read.csv("ALSFRS_slope.csv")
str(slope)
summary(slope)
dim(slope) # 3096 patients 
slope = rename(slope, slope = ALSFRS_slope)

# Import ALSFRS_original datasets 
alsfrs_orig = read.csv("ALSFRS_original.csv") 
str(alsfrs_orig)
length(unique(alsfrs_orig$SubjectID)) # 6514 patients 
summary(alsfrs_orig)

## all patients with ALSFRS_slope value (n=3096) 
## had at least one ALSFRS_original score measured    
## in contrast, only about a half of the patients had
## ALSFRS_revised score measured.  
## we will use ALSFRS original dataset 
## in order to build prediction model for ALSFRS slope. 

# alsfrs_rev = read.csv("ALSFRS_revised.csv")
# length(unique(alsfrs_orig$SubjectID)) # 6514 patients 
# length(unique(alsfrs_rev$SubjectID)) # 3412 patients 
# alsfrs_both_subj = intersect(unique(alsfrs_orig$SubjectID), 
#                              unique(alsfrs_rev$SubjectID))
# length(alsfrs_both_subj) # 3412 patients 
# all(alsfrs_both_subj == unique(alsfrs_rev$SubjectID))
# length(intersect(alsfrs_slope$SubjectID, alsfrs_orig$SubjectID)) # 3096 patients
# length(intersect(alsfrs_slope$SubjectID, alsfrs_rev$SubjectID)) # 1594 patients

demo = read.csv("demographic.csv")
dim(demo) # 8653 patients 
str(demo)
demo = demo %>% select(-Race)
demo$Gender = factor(demo$Gender)

hx = read.csv("als_hx.csv")
dim(hx) # 4454 patients 
str(hx)
hx = hx %>%
  filter(onset_delta < diag_delta)
dim(hx) # 4427 patients
hx = hx %>%
  mutate(onset2dx = diag_delta - onset_delta, 
         diag_delta = -diag_delta, onset_delta = -onset_delta)
hx$onset_site = factor(hx$onset_site)

# For time resolved featurese; calculate mean scores for each item, 
# dimension (bulbar, motor, respiratory), and total scores 
# for bulbar scores, we exclude the item for salivation because 
# there are some symptomatic treatments for the symptom 

temp = alsfrs_orig[((alsfrs_orig$feature_delta)/365)*12 <= 3,]
range(temp$feature_delta)
temp = temp[((temp$feature_delta)/365)*12 >= 0,]
length(unique(temp$SubjectID)) # 6507 patients 
table(table(temp$SubjectID))
temp <- temp %>%
  group_by(SubjectID) %>%
  summarise(ALSFRS_Total = mean(ALSFRS_Total), 
            Q1_Speech = mean(Q1_Speech), 
            Q2_Salivation = mean(Q2_Salivation), 
            Q3_Swallowing = mean(Q3_Swallowing), 
            Q4_Handwriting = mean(Q4_Handwriting), 
            Q5_Cutting = mean(Q5_Cutting), 
            Q5a_Cutting_without_Gastrostomy = mean(Q5a_Cutting_without_Gastrostomy),
            Q5b_Cutting_with_Gastrostomy = mean(Q5b_Cutting_with_Gastrostomy),
            Q6_Dressing_and_Hygiene = mean(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed = mean(Q7_Turning_in_Bed),
            Q8_Walking = mean(Q8_Walking),
            Q9_Climbing_Stairs = mean(Q9_Climbing_Stairs),
            Q10_Respiratory = mean(Q10_Respiratory), 
            Gastrostomy = ifelse(is.na(Q5b_Cutting_with_Gastrostomy), F, T)
            ) %>%
  mutate(Bulbar = Q1_Speech + Q3_Swallowing, 
         Motor = Q4_Handwriting + Q5_Cutting + Q6_Dressing_and_Hygiene + Q7_Turning_in_Bed + Q8_Walking + Q9_Climbing_Stairs, 
         Gastrostomy = ifelse(is.na(Q5a_Cutting_without_Gastrostomy) & 
                                is.na(Q5b_Cutting_with_Gastrostomy), NA, Gastrostomy))

alsfrs = temp

fvc = read.csv("fvc.csv")
temp = fvc %>%
  filter((feature_delta)/365*12 <= 3) %>%
  filter(feature_delta >= 0) # 7032 patients 
range(temp$feature_delta)
temp = temp %>%
  group_by(SubjectID) %>%
  summarise(fvc_percent = mean(fvc_percent))
fvc = temp

# Merge datasets 
temp = merge(slope, demo, by = "SubjectID", all.x = T) 
temp2 = merge(temp, hx, by = "SubjectID", all.x = T)
temp3 = merge(temp2, alsfrs, by = "SubjectID", all.x = T)
temp4 = merge(temp3, fvc, by="SubjectID", all.x = T) 

dim(temp4)
summary(temp4)
df = temp4 %>%
  select(-c(SubjectID, Q5a_Cutting_without_Gastrostomy, Q5b_Cutting_with_Gastrostomy))

# Explore missing data before imputation 
summary(df)
df$Gender = factor(df$Gender)

ini = mice(df, maxit = 0)
table(ini$nmis) # number of variables with missing values

mis = VIM::aggr(df, 
     numbers = T, cex.axis = .4) # plot the amount of missing values in each variables
mis

# Remove variables with high proportion of missing values 
df = df %>%
  filter(!is.na(diag_delta))

# Remove variables onset_site because 
# it is redundant (information in ALSFRS item scores) 
df = df %>% 
  select(-onset_site)

# Muliple imputation using mice package 
set.seed(999)
imp = mice(df, print = F)
# class(imp)
# imp$pred
imp$method
df1 = complete(imp, 1) # first imputed dataset
# temp2 = complete(imp, "long")
summary(df1)

# plot(imp)
# stripplot(imp)

# Evaluate the performance using cross validation 
plot(density(df1$slope))
summary(df1$slope)

# dummy variables 
df1$Gender = ifelse(df1$Gender == "F", 0, 1)
# scaling 
temp = df1 %>%
  mutate(across(-c(slope), scale))

summary(temp)
df1 = temp

set.seed(1)
samp = createDataPartition(df1$slope, p = 0.8, list = F)
training = df1[samp,]
testing = df1[-samp,]

# elastic net 
trainControl = trainControl(method = 'cv', number = 10) # 10-fold CV
tuneGrid <- expand.grid(
  alpha = 1, # lasso regression 
  lambda = seq(0.0001, 1, length = 5)
)
lm1 = train(slope ~ ., 
            data = df1, 
            method = 'glmnet', # elastic net
            preProcess = c("center", "scale"),
            tuneGrid = tuneGrid, # hyperparameters; alpha, labmda
            trControl = trainControl) 
lm1 
plot(lm1)
plot(lm1$finalModel) 
lm1$bestTune # RMSE used to select the optimal model
lm1$results  # each combination of alpha and lambda, average of CV
coef(lm1$finalModel, lm1$bestTune$lambda)

# Performance in a test set 
slope_obs = testing$slope
df4pred = subset(testing, select = -c(slope))
slope_pred_lm = predict(lm1, newdata = df4pred)
df_eval = data.frame(cbind(slope_obs, slope_pred))

p1 = ggplot(df_eval, aes(slope_obs, slope_pred_lm)) + 
  geom_point() + 
  geom_abline(slope = 1) + 
  xlim(c(-3,1)) + 
  ylim(c(-3,1))

rmse(df_eval$slope_obs, df_eval$slope_pred)
cor.test(df_eval$slope_obs, df_eval$slope_pred) 
# How to improve performance of the prediction model???

# random foreset model 
tg = data.frame(mtry = seq(2,10,by=2))
rf1 = train(slope ~ ., data = df1, method = "rf",
             trControl = tc, tuneGrid = tg)
rf1
plot(rf1)
rf1$results
rf1$finalModel

slope_pred_rf = predict(rf1, newdata = df4pred)
df_eval = data.frame(cbind(slope_obs, slope_pred_rf))
p2 = ggplot(df_eval, aes(slope_obs, slope_pred_rf)) + 
  geom_point() + 
  geom_abline(slope = 1) + 
  xlim(c(-3,1)) + 
  ylim(c(-3,1))

png("/Users/hong/Documents/GitHub/PROACT/images/cor_lm_rf.png")
grid.arrange(p1, p2, ncol=2)
dev.off()

# model performance: comparisons
model_list = list(lm = lm1, rf = rf1)
res = resamples(model_list) # number of resamples: 10
res
summary(res) 
summary(diff(res)) 

png("/Users/hong/Documents/GitHub/PROACT/images/model_comparisons.png")
bwplot(res) 
dev.off()

compare_models(lm1, rf1) 
compare_models(lm1, rf1, metric = "Rsquared") 
compare_models(lm1, rf1, metric = "RMSE") 
compare_models(lm1, rf1, metric = "MAE") 


