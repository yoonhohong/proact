load("~/Documents/ALSmaster_data/Predict_class.RData")

# predict class ALSFRS slope 

library(dplyr)
library(caret)
library(doMC)
registerDoMC(cores = 4)

setwd("~/Documents/ALSmaster_data")

alsfrs.slope = read.csv("ALSFRS_slope_after3mo.csv")
q4 = quantile(alsfrs.slope$alsfrs_slope, probs = seq(0,1,0.25))
alsfrs.slope %>%
  mutate(subgroup = cut(alsfrs.slope$alsfrs_slope, breaks = q4, 
                        include.lowest = T, right = F)) -> alsfrs.slope

# Target variable, 3-12 mo ALSFRS_Total slope
y = alsfrs.slope$subgroup
levels(y) = list(fast = "[-10.7,-1.26)", medium = c("[-1.26,-0.717)","[-0.717,-0.331)"), 
                 slow = "[-0.331,7.5]")
names(y) = alsfrs.slope$SubjectID

# Read predictors data; imputed and dummied 
proact = read.csv("PROACT_imputed_dummied.csv")
proact.sub = subset(proact, SubjectID %in% alsfrs.slope$SubjectID)

x = proact.sub[,2:dim(proact.sub)[2]]
x = as.matrix(x); rownames(x) = proact.sub$SubjectID

o1 = order(names(y)); y = y[o1]
o2 = order(rownames(x)); x = x[o2,]

df = as.data.frame(cbind(x,y))
colnames(df)[117] = "class"
df$class = factor(df$class)
levels(df$class) = c("fast", "medium", "slow") 

# classification 

set.seed(1)
inTrain = createDataPartition(y = y, p=0.75, list = F)
training = df[inTrain,]
testing = df[-inTrain,]


# glmnet; lasso and ridge
ctrl = trainControl(method = "CV", # by default, 10 fold CV 
                    number = 4, 
                    classProbs = T)  

grid.logit = expand.grid(.alpha=c(0,1), .lambda = seq(0.000001,0.1,length = 50))

logitfit = train(class ~ .,
                 data = training, 
               method = 'glmnet',
               trControl = ctrl,
               tuneGrid = grid.logit)

plot(logitfit)
attributes(logitfit)
coef(logitfit$finalModel, s = logitfit$bestTune$lambda) 

pred.class = predict(logitfit, newdata = testing)
pred.prob = predict(logitfit, newdata = testing, type = "prob")
  
# confusion matrix 
confusion.matrix.glm = confusionMatrix(pred.class, testing$class)
confusion.matrix.glm
perf.byClass = as.data.frame(t(round(confusion.matrix.glm$byClass, 2)))
write.csv(perf.byClass, "Performance_byClass_lasso.csv", quote = F, row.names = T)

varImp.logit = varImp(logitfit, scale = F) # 
plot(varImp.logit, 20)

# random forest classification 
grid.rfc = expand.grid(.mtry=seq(4,20,4), .min.node.size = 5, .splitrule = "gini")
# splitrule, 'variance' in case of regression  

rfcfit = train(class ~ .,
              data = training, 
              method = 'ranger', 
              trControl = ctrl,
              tuneGrid = grid.rfc,
              importance = "permutation" # or "impurity"
            )

plot(rfcfit)

# predict 
pred.class = predict(rfcfit, newdata = testing)
pred.prob = predict(rfcfit, newdata = testing, type = "prob")

# confusion matrix 
confusion.matrix.RF = confusionMatrix(pred.class, testing$class)
confusion.matrix.RF 
perf.byClass.RF = as.data.frame(t(round(confusion.matrix.RF$byClass, 2)))
write.csv(perf.byClass.RF, "Performance_byClass_RF.csv", quote = F, row.names = T)

varImp.rfcfit = varImp(rfcfit, scale = F) # 
plot(varImp.rfcfit, 20)

# compare performance 

resamps = resamples(list(logit.lasso = logitfit, random.forest = rfcfit))
summary(resamps)
bwplot(resamps)




