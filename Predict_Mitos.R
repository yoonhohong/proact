# load library 
library(mlr)
library(Hmisc)
library(doMC)
registerDoMC(cores = 4)

setwd("~/Documents/ALSmaster_data")

# Predict LOA in mitos
mitos.movement = read.csv("Movement_all.csv")
colnames(mitos.movement) = c("SubjectID", "time_event", "status")

mitos.swallowing = read.csv("Swallowing_all.csv")
colnames(mitos.swallowing) = c("SubjectID", "time_event", "status")

mitos.communicating = read.csv("Communicating_all.csv")
colnames(mitos.communicating) = c("SubjectID", "time_event", "status")

mitos.breathing = read.csv("Breathing_all.csv")
colnames(mitos.breathing) = c("SubjectID", "time_event", "status")

# preditors
proact = read.csv("PROACT_imputed_dummied.csv")

# merge 
df.comm = merge(mitos.communicating, proact, by = "SubjectID")
df.comm = df.comm[,-1]

# split dataset
n = sample(dim(df.comm)[1], size = 0.75*dim(df.comm)[1])
trainset.comm = df.comm[n,]
testset.comm = df.comm[-n,]

# task 
surv.task.comm = makeSurvTask(data = trainset.comm, target = c("time_event", "status"))

# learner 
surv.lrn.coxph.comm = makeLearner("surv.coxph")
surv.lrn.rf.comm = makeLearner("surv.randomForestSRC")

# resample 

rdsc = makeResampleDesc("CV", iters = 10)

# benchmark 

lrns.comm = list(surv.lrn.coxph.comm, surv.lrn.rf.comm)
bchmark.comm = benchmark(lrns.comm, tasks = surv.task, resamplings = rdsc)  
bchmark.comm
plotBMRBoxplots(bchmark.comm) 

