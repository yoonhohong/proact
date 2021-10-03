# predict the slope of ALSFRS total scores 
# features: Age, Gender, onset_delta, ALSFRS total score, 
# ALSFRS item and dimension score, FVC  
# algorithm: lasso linear regression, random forest 

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT") 

library(tidyverse)
library(ggExtra)

library(caret) # machine learning 
library(gridExtra)
library(ModelMetrics) 

# read rds object 
df = readRDS("slope_rev_selected_features.RDS")

p = ggplot(df, aes(slope_total, slope_3to12)) + 
  geom_point() + 
  xlab("Slope over the first 3 month") + 
  ylab("Slope over the next 3 to 12 months")

ggMarginal(p, type = "histogram")

set.seed(1)
samp = createDataPartition(df$slope_3to12, p = 0.8, list = F) # list = F to return matrix
training = df[samp,]
testing = df[-samp,]

# elastic net 
trainControl = trainControl(method = 'cv', number = 10) 
# 10-fold CV
tuneGrid <- expand.grid(
  alpha = 1, # lasso regression 
  lambda = seq(0.000001, 1, length = 5)
)
lm = train(slope_3to12 ~ ., 
            data = training, 
            method = 'glmnet', # elastic net
            tuneGrid = tuneGrid, # hyperparameters; alpha, labmda
            trControl = trainControl) 
plot(lm)
plot(lm$finalModel) 
lm$bestTune # RMSE used to select the optimal model
lm$results  # each combination of alpha and lambda, average of CV
coef(lm$finalModel, lm$bestTune$lambda)

# Performance in a test set 
slope_obs = testing$slope_3to12
df4pred = subset(testing, select = -c(slope_3to12))
slope_pred = predict(lm, newdata = df4pred)
df_eval = data.frame(cbind(slope_obs, slope_pred))

p1 = ggplot(df_eval, aes(slope_obs, slope_pred)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(slope = 1) + 
  xlim(c(floor(min(df_eval$slope_obs)),
         ceiling(max(df_eval$slope_obs)))) + 
  ylim(c(floor(min(df_eval$slope_obs)),
         ceiling(max(df_eval$slope_obs)))) + 
  ggtitle("Lasso linear regression") + 
  theme(plot.title = 
          element_text(hjust = 0.5))
p1

rmse(df_eval$slope_obs, df_eval$slope_pred) # ModelMetrics package
cor.test(df_eval$slope_obs, df_eval$slope_pred) 

# random foreset model 
tg = data.frame(mtry = seq(2,10,by=2))
rf = train(slope_3to12 ~ ., data = training, 
           method = "rf",
           trControl = trainControl, 
           tuneGrid = tg)
plot(rf)
rf$results
rf$finalModel

slope_pred = predict(rf, newdata = df4pred)
slope_obs = testing$slope_3to12
df_eval2 = data.frame(cbind(slope_obs, slope_pred))
p2 = ggplot(df_eval2, aes(slope_obs, slope_pred)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(slope = 1) + 
  xlim(c(floor(min(df_eval2$slope_obs)),
         ceiling(max(df_eval2$slope_obs)))) + 
  ylim(c(floor(min(df_eval2$slope_obs)),
         ceiling(max(df_eval2$slope_obs)))) + 
  ggtitle("Random forest") + 
  theme(plot.title = 
        element_text(hjust = 0.5))
p2

rmse(df_eval2$slope_obs, df_eval2$slope_pred)
cor.test(df_eval2$slope_obs, df_eval2$slope_pred) 

grid.arrange(p1, p2, ncol=2)

# compare performance of the models 
model_list = list(lm = lm, rf = rf)
res = resamples(model_list) # number of resamples: 10 (number of CV)
summary(res) 
summary(diff(res)) 

# /Users/hong/Documents/GitHub/PROACT/images
# model_comparison_regression_1.pdf   
bwplot(res) # box and whisker plot (lattice)   

# statistical comparison between models   
compare_models(lm, rf) # by default lm$metric[1] (eg, RMSE)
compare_models(lm, rf, metric = "Rsquared") 
compare_models(lm, rf, metric = "RMSE") 
compare_models(lm, rf, metric = "MAE") 

# How to improve performance of the prediction model???

