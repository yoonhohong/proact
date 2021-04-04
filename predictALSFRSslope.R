# predict the slope of ALSFRS total scores 
# features: Age, Gender, onset_delta, ALSFRS total score, 
# ALSFRS item and dimension score, FVC  
# algorithm: lasso linear regression, random forest 

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT") 
# dropbox folder to import datasets 

library(dplyr)
library(ggplot2)
library(mice) # multiple imputation 
library(VIM) # visualization of missing data
library(caret) # machine learning 
library(ModelMetrics) 
library(gridExtra)


# Import ALSFRS_slope dataset (target variable, 3-12 mo slope)  
slope = read.csv("ALSFRS_slope.csv")
dim(slope) # 3096 patients 
slope = rename(slope, slope = ALSFRS_slope)

# Import predicting feature dataset 
df = read.csv("PROACT_preprocessed.csv") # need imputed dataset   

# 
# Explore missing data before imputation 
summary(df)

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


