# load library 
library(mlr)
library(Hmisc)
library(doMC)
registerDoMC(cores = 4)

setwd("~/Documents/ALSmaster_data")
load("~/Documents/ALSmaster_data/Predict_survival.RData")

# Predict survival 

surv = read.csv("Survival_all.csv")
proact = read.csv("PROACT_imputed_dummied.csv")

df = merge(surv, proact, by = "SubjectID")
df = df[,-1]

# split dataset
n = sample(dim(df)[1], size = 0.75*dim(df)[1])
trainset = df[n,]
testset = df[-n,]

# task 
surv.task = makeSurvTask(data = trainset, target = c("time_event", "status"))

# learner 
surv.lrn.coxph = makeLearner("surv.coxph")
surv.lrn.rf = makeLearner("surv.randomForestSRC")

# train 
model.coxph = train(surv.lrn.coxph, surv.task)

# tuning hyperparameters 

model.rf = train(surv.lrn.rf, surv.task)
                    
# predict & performance
pred.coxph = predict(model.coxph, newdata = testset)
pred.coxph.df = as.data.frame(pred.coxph)
performance(pred.coxph)

pred.rf = predict(model.rf, newdata = testset)
pred.rf.df = as.data.frame(pred.rf)
performance(pred.rf)                         

# resample 

rdsc = makeResampleDesc("CV", iters = 10)
cv.coxph = resample("surv.coxph", surv.task, 
         resampling = rdsc, 
         keep.pred = F)                         
cv.rf = resample("surv.randomForestSRC", surv.task, 
                    resampling = rdsc, 
                    keep.pred = F)                      
                         
# benchmark 

lrns = list(surv.lrn.coxph, surv.lrn.rf)
bchmark = benchmark(lrns, tasks = surv.task, resamplings = rdsc)  
plotBMRBoxplots(bchmark)                         

# prediction error curve 

library(pec)
library(survival)
library(randomForestSRC)
library(rms) 
library(prodlim)
library(party)

fitform = Surv(time_event, status) ~ . 

fitcox <- coxph(fitform, data=trainset)
fitcforest = pecCforest(fitform, data = trainset, 
                        controls = cforest_classical(ntree=1000))

set.seed(13)

pcox = predictSurvProb(fitcox, newdata = testset, 
                       times = 30*12)
pcf = predictSurvProb(fitcforest, newdata = testset, 
                      times = 30*12)

extends <- function(...)TRUE

par(mfrow = c(2,4))
lapply(1:8, function(x){
  plotPredictSurvProb(fitcox,newdata=testset[x,],lty=1)
  plotPredictSurvProb(fitcforest, newdata=testset[x,], add=TRUE, lty=2)
})


fitpec = pec(list("selectcox" = fitcox, "cforest" = fitcforest), 
             data = trainset, 
             formula = Surv(time_event,status)~1, 
             splitMethod = "cv10", B = 4, # 10-fold CV, repeated 4 times 
             keep.index = T, keep.matrix = T)

dev.off()
plot(fitpec)
crps.t1000 = crps(fitpec, times = 1000)
# continuous rank probability score 
# computes integrated Brier scores 






