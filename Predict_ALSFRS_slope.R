load("~/Documents/ALSmaster_data/Predict_ALSFRS_slope.RData")

# Predict ALSFRS slope 3 to 12 mo, with baseline 3 mo data 

require(caret)
library(doMC)
library(ModelMetrics)

setwd("~/Documents/ALSmaster_data")

# Predict 3-12 mo slope 

# devtools::install_github('topepo/caret/pkg/caret')

# parallel processing 
registerDoMC(cores = 4)

# Read slope data 
alsfrs.slope = read.csv("ALSFRS_slope_after3mo.csv")

# Target variable, 3-12 mo ALSFRS_Total slope
y = alsfrs.slope$alsfrs_slope
names(y) = alsfrs.slope$SubjectID

# Read predictors data; imputed and dummied 
proact = read.csv("PROACT_imputed_dummied.csv")
proact.sub = subset(proact, SubjectID %in% alsfrs.slope$SubjectID)

x = proact.sub[,2:dim(proact.sub)[2]]
x = as.matrix(x); rownames(x) = proact.sub$SubjectID

o1 = order(names(y)); y = y[o1]
o2 = order(rownames(x)); x = x[o2,]

alsfrs.slope.df = as.data.frame(cbind(x,y))
colnames(alsfrs.slope.df)[117] = "slope_reference" # 117

# remove redundant features 
# xcor = cor(x)
# corrplot(xcor, order = "hclust", addrect = 3) 
# highlyCorrelated = findCorrelation(xcor, 0.75)
# x = x[,-highlyCorrelated]

# Partition data: training and testing 
set.seed(1)
inTrain = createDataPartition(y = alsfrs.slope.df$slope_reference, p=0.75, list = F)
training = alsfrs.slope.df[inTrain,]
testing = alsfrs.slope.df[-inTrain,]


# glmfit 

ctrl = trainControl(method = "CV", # by default, 10 fold CV 
                    number = 2, 
                    allowParallel = T)  

grid.glm = expand.grid(.alpha=c(0,1), .lambda = seq(0.000001,0.1,length = 50))

glmfit = train(slope_reference ~ .,
               data = training, 
               method = 'glmnet', 
               trControl = ctrl,
               tuneGrid = grid.glm, 
               metric = "RMSE")

# plot performance measures with tuning parameters values 
plot(glmfit)
attributes(glmfit)
coef(glmfit$finalModel, s = glmfit$bestTune$lambda) 

# prediction 
pred = predict(glmfit, newdata = testing)

# scatter plot 
df.glm = as.data.frame(cbind(testing, pred))

ggplot(data = df.glm, aes(x=slope_reference, y=pred)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = 'lm', linetype = 'dashed') + 
  theme_bw() + 
  labs(x="Reference", 
       y="Prediction", 
       title = "Slope: GLM lasso") +
  xlim(-5,2) + ylim(-5,2)

cor.test(df.glm$slope_reference, df.glm$pred)
rmse(df.glm$slope_reference, df.glm$pred)

# random forest 
grid.rf = expand.grid(.mtry=seq(4,20,4), .min.node.size = 5, .splitrule = "variance")
# splitrule, 'gini' in case of classification  

rffit = train(slope_reference ~ .,
              data = training, 
              method = 'ranger', 
              trControl = ctrl,
              tuneGrid = grid.rf,
              importance = "permutation", # or "impurity"
              metric = "RMSE")

plot(rffit)

rffit$bestTune
rf.model = rffit$finalModel

pred = predict(rffit, newdata = testing)

# scatter plot 
df.rf = as.data.frame(cbind(testing, pred))

ggplot(data = df.rf, aes(x=slope_reference, y=pred)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth(method = 'lm', linetype = 'dashed') + 
  theme_bw() + 
  labs(x="Reference", 
       y="Prediction", 
       title = "Slope: Random forest") +
  xlim(-5,2) + ylim(-5,2)

cor.test(df.rf$slope_reference, df.rf$pred)
rmse(df.rf$slope_reference, df.rf$pred)

# compare performance measures btw models 
resamps = resamples(list(GLM.lasso = glmfit, RF = rffit))
summary(resamps)
bwplot(resamps)

# variable importance 
varImp.rf = varImp(rffit, scale = F) 
plot(varImp.rf, 20)
varImp.glm = varImp(glmfit, scale = F) # 
plot(varImp.glm, 20) # importance == coefficients? should be scale first before train, 'cause 
# train returns beta coefficients on original scales... 

# dependency plots....


# # feature selection 
# subsets <- c(5, 10, 15, 20, 25)
# 
# ctrl <- rfeControl(functions = caretFuncs,
#                    method = "repeatedcv",
#                    repeats = 5,
#                    verbose = FALSE)
# # glmnet; RFE 
# 
# glmnetProfile = rfe(x,y,sizes=subsets, rfeControl = ctrl)
# glmnetProfile 
# 
# # random forest; RFE (recursive feature elimination)
# ctrl$functions <- rfFuncs
# ctrl$returnResamp <- "all"
# rfProfile <- rfe(x, y, sizes = subsets, rfeControl = ctrl)
# rfProfile
# 
# plot(rfProfile, type = c("g", "o"))
# predictors(rfProfile)
# rfProfile$fit
# varImp(rfProfile)
