# predict the slope of ALSFRS total scores 
# features: Age, Gender, onset_site, onset_delta, ALSFRS total score, FVC  
# algorithm: lasso linear regression

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

library(dplyr)
library(ggplot2)
library(mice)
library(VIM)
library(caret)
library(ModelMetrics)
# Import ALSFRS_slope dataset; target variable  
slope = read.csv("ALSFRS_slope.csv")
dim(slope) # 3096 patients 
slope = rename(slope, slope = ALSFRS_slope)

# Import ALSFRS_original datasets 
alsfrs_orig = read.csv("ALSFRS_original.csv") # 6514 patients 

## all patients with ALSFRS_slope value (n=3096) 
## had at least one ALSFRS_original scores measured    
## in contrast, only about a half of the patients also had
## ALSFRS_revised scored measured.  
## therefore, we will use ALSFRS original dataset 
## in order to build prediction models for ALSFRS slope. 

# alsfrs_rev = read.csv("ALSFRS_revised.csv")
# length(unique(alsfrs_orig$SubjectID)) # 6514 patients 
# length(unique(alsfrs_rev$SubjectID)) # 3412 patients 
# alsfrs_both_subj = intersect(unique(alsfrs_orig$SubjectID), 
#                              unique(alsfrs_rev$SubjectID))
# length(alsfrs_both_subj) # 3412 patients 
# all(alsfrs_both_subj == unique(alsfrs_rev$SubjectID))
# length(intersect(alsfrs_slope$SubjectID, alsfrs_orig$SubjectID)) # 3096 patients
# length(intersect(alsfrs_slope$SubjectID, alsfrs_rev$SubjectID)) # 1594 patients

temp = alsfrs_orig[alsfrs_orig$feature_delta == 0,]
dim(temp) # 4676 patients 

alsfrs = temp %>%
  select(SubjectID, ALSFRS_Total)
alsfrs = rename(alsfrs, alsfrs = ALSFRS_Total)

demo = read.csv("demographic.csv")
demo = demo %>%
  select(-Race)
dim(demo) # 8653 patients 
demo = rename(demo, age = Age, gender = Gender)
demo$gender = factor(demo$gender)

hx = read.csv("als_hx.csv")
hx = hx %>% 
  select(SubjectID, onset_delta, onset_site) %>%
  mutate(onset_delta = -onset_delta)
dim(hx) # 4454 patients 
hx$onset_site = factor(hx$onset_site)

fvc = read.csv("fvc.csv")
temp = fvc %>%
  filter(feature_delta == 0) # 7032 patients 
fvc = temp %>% select(SubjectID, fvc_percent)
fvc = rename(fvc, fvc = fvc_percent) 

temp = merge(slope, demo, by = "SubjectID", all.x = T) 
temp2 = merge(temp, hx, by = "SubjectID", all.x = T)
temp3 = merge(temp2, alsfrs, by = "SubjectID", all.x = T)
temp4 = merge(temp3, fvc, by="SubjectID", all.x = T) 

dim(temp4)
summary(temp4)
boxplot(slope ~ onset_site, data = temp4)
temp4$onset_site = factor(temp4$onset_site, 
                          levels = c("Limb and Bulbar", 
                                     "Bulbar", 
                                     "Limb", 
                                     "Other"))
df = subset(temp4, select = -c(SubjectID))


# Missing data imputation 
VIM::aggr(df, 
     numbers = T, cex.axis = .9)
# delete subjects with no record about onset_delta or onset_site
temp2 = df %>%
  filter(!is.na(onset_delta)) %>%
  filter(!is.na(onset_site))
dim(temp2) # 1653 patients 

aggr(temp2, 
     numbers = T, cex.axis = 0.9,
     ylab = c("Proportion of missing data", 
              "Pattern of missing data"))
df = temp2

# Muliple imputation using mice package 
set.seed(999)
imp = mice(df, print = F)
class(imp)
imp$pred
imp$method
temp = complete(imp, 1) # first imputed dataset
temp2 = complete(imp, "long")

plot(imp)
stripplot(imp)

# fit linear regression 
# fit.lm = with(imp, lm(slope ~ age + gender + 
#                         onset_delta + onset_site + 
#                       alsfrs + fvc))
# class(fit.lm) # mira; multiply imputate repeated analyses
# ls(fit.lm) 
# summary(fit.lm$analyses[[2]])
# 
# pool.fit.lm = pool(fit.lm)
# summary(pool.fit.lm)

# Evaluate the performance using cross validation 
df1 = complete(imp, 1) # select the first imputate dataset arbitrarily
plot(density(df1$slope))
summary(df1$slope)

set.seed(1)
samp = createDataPartition(df1$slope, p = 0.8, list = F)
training = df1[samp,]
testing = df1[-samp,]
tc = trainControl(method = 'cv', number = 10)
lm1 = train(slope ~ ., data = df1, method = 'lm', 
      trControl = tc)
lm1$results  # 10-fold CV
lm1$finalModel # full training dataset 

# Performance in a test set 
slope_obs = testing$slope
df4pred = subset(testing, select = -c(slope))
slope_pred = predict(lm1, newdata = df4pred)
df_eval = data.frame(cbind(slope_obs, slope_pred))
ggplot(df_eval, aes(slope_obs, slope_pred)) + 
  geom_point() + 
  geom_abline(slope = 1, ) + 
  xlim(c(-3,1)) + 
  ylim(c(-3,1))

rmse(df_eval$slope_obs, df_eval$slope_pred)
mae(df_eval$slope_obs, df_eval$slope_pred)

# How to improve performance of the prediction model???

# tg = data.frame(mtry = seq(2,10,by=2))
# rf1 = train(slope ~ ., data = df1, method = "rf", 
#             trControl = tc, tuneGrid = tg)

# rf1$results  
# rf1$finalModel 

# model_list = list(lm = lm1, rf = rf1)
# res = resamples(model_list) 
# summary(res)
# compare_models(lm1, rf1)




