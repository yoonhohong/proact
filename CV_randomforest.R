#library(survcomp)
library(Hmisc)
library(ranger)
library(randomForestSRC)
#library(rpart)
#library(party)
library(prodlim)
#library(pec)
library(glmnet)
library(CoxBoost)



movement_merge3 <- merge(movement_survival_sub,allpatientsfeature_nonimputed,all.x=TRUE)
movement_final_sub<- subset(movement_merge3, select=-SubjectID)
swallowing_merge3 <- merge(swallowing_survival_sub,allpatientsfeature_nonimputed,all.x=TRUE)
swallowing_final_sub<- subset(swallowing_merge3, select=-SubjectID)
communicating_merge3 <- merge(communicating_survival_sub,allpatientsfeature_nonimputed,all.x=TRUE)
communicating_final_sub<- subset(communicating_merge3, select=-SubjectID)
breathing_merge3 <- merge(breathing_survival_sub,allpatientsfeature_nonimputed,all.x=TRUE)
breathing_final_sub<- subset(breathing_merge3, select=-SubjectID)
death_merge3 <- merge(death_survival_sub,allpatientsfeature_nonimputed,all.x=TRUE)
death_final_sub<- subset(death_merge3, select=-SubjectID)

# 각 domain에서 성능 plot 함수
cvplot <- function(x1,x2,x3,x4,x5){
  bbb=data.frame("Domain"=c("Movement","Swallowing","Communicating","Breathing","Death"),
                 "mean"=c(mean(x1),mean(x2),mean(x3),mean(x4),mean(x5)),
  "inf"=c(mean(x1)-2.262*sd(x1)/sqrt(length(x1)),mean(x2)-2.262*sd(x2)/sqrt(length(x2)),mean(x3)-2.262*sd(x3)/sqrt(length(x3)),mean(x4)-2.262*sd(x4)/sqrt(length(x4)),mean(x5)-2.262*sd(x5)/sqrt(length(x5))),
  "sup"=c(mean(x1)+2.262*sd(x1)/sqrt(length(x1)),mean(x2)+2.262*sd(x2)/sqrt(length(x2)),mean(x3)+2.262*sd(x3)/sqrt(length(x3)),mean(x4)+2.262*sd(x4)/sqrt(length(x4)),mean(x5)+2.262*sd(x5)/sqrt(length(x5))))
  ggplot(bbb,aes(x=mean,y=Domain))+geom_errorbarh(bbb, mapping=aes(y=Domain, xmin=inf, xmax=sup),height=0.1)+geom_point(bbb,mapping=aes(y=Domain, x=mean),size=2)+geom_text(bbb,mapping=aes(y=as.numeric(Domain)+0.2,x=mean,label=round(mean,2)))
  # 10 fold면 2.776, 5 fold면 2.262
}                       

v1 <- rep(0,5)
v2 <- rep(0,5)
v3 <- rep(0,5)
v4 <- rep(0,5)
v5 <- rep(0,5)

movement_random <- movement_final_sub[sample(1:nrow(movement_final_sub)),]
for (i in 1:5){
  indices <- (((i-1) * round((1/5)*nrow(movement_random))) + 1):(min(((i*round((1/5) * nrow(movement_random)))),nrow(movement_random)))
  training <- movement_random[-indices,]
  test <- movement_random[indices,]
  #ans <- gammaImpute(formula(paste("Surv(movement_whenevent,movement_isevent==1)","~",paste(names(training)[c(3:15)],collapse="+"))),data=training,m=2 , gamma.factor=0.5,DCO.time=max(training$movement_whenevent))
  #b=ExtractSingle(ans,index=1)
  #b=b$data
  #training=subset(b,select=-c(1:15,internalDCO.time,internal_gamma_val))
  #names(training)[c(length(names(training))-1,length(names(training)))]=c("when","is")
  #model=coxph(Surv(movement_whenevent,movement_isevent==1)~.,data=training)
  print(i)
  model=rfsrc(Surv(movement_whenevent,movement_isevent==1)~.,data=training,na.action="na.impute",nimpute=3)
  v1[i]=survConcordance(Surv(movement_whenevent,movement_isevent)~predict(model,test,na.action="na.impute")$predicted,test)$concordance
  }

swallowing_random <- swallowing_final_sub[sample(1:nrow(swallowing_final_sub)),]
for (i in 1:5){
  indices <- (((i-1) * round((1/5)*nrow(swallowing_random))) + 1):(min(((i*round((1/5) * nrow(swallowing_random)))),nrow(swallowing_random)))
  training <- swallowing_random[-indices,]
  test <- swallowing_random[indices,]
  #ans <- gammaImpute(formula(paste("Surv(swallowing_whenevent,swallowing_isevent==1)","~",paste(names(training)[c(3:15)],collapse="+"))),data=training,m=2 , gamma.factor=2,DCO.time=max(training$swallowing_whenevent))
  #b=ExtractSingle(ans,index=1)
  #b=b$data
  #training=subset(b,select=-c(1:15,internalDCO.time,internal_gamma_val))
  #names(training)[c(length(names(training))-1,length(names(training)))]=c("when","is")
  #model=coxph(Surv(when,is==1)~.,data=training)
  print(i)
  model=rfsrc(Surv(swallowing_whenevent,swallowing_isevent==1)~.,data=training,na.action="na.impute",nimpute=3)
  v2[i]=survConcordance(Surv(swallowing_whenevent,swallowing_isevent)~predict(model,test,na.action="na.impute")$predicted,test)$concordance
  
  }
communicating_random <- communicating_final_sub[sample(1:nrow(communicating_final_sub)),]
for (i in 1:5){
  indices <- (((i-1) * round((1/5)*nrow(communicating_random))) + 1):(min(((i*round((1/5) * nrow(communicating_random)))),nrow(communicating_random)))
  training <- communicating_random[-indices,]
  test <- communicating_random[indices,]
  #ans <- gammaImpute(formula(paste("Surv(communicating_whenevent,communicating_isevent==1)","~",paste(names(training)[c(3:15)],collapse="+"))),data=training,m=2 , gamma.factor=2,DCO.time=max(training$communicating_whenevent))
  #b=ExtractSingle(ans,index=1)
  #b=b$data
  #training=subset(b,select=-c(1:15,internalDCO.time,internal_gamma_val))
  #names(training)[c(length(names(training))-1,length(names(training)))]=c("when","is")
  #model=coxph(Surv(when,is==1)~.,data=training)
  print(i)
  model=rfsrc(Surv(communicating_whenevent,communicating_isevent==1)~.,data=training,na.action="na.impute",nimpute=3)
  v3[i]=survConcordance(Surv(communicating_whenevent,communicating_isevent)~predict(model,test,na.action="na.impute")$predicted,test)$concordance
  
}
breathing_random <- breathing_final_sub[sample(1:nrow(breathing_final_sub)),]
for (i in 1:5){
  indices <- (((i-1) * round((1/5)*nrow(breathing_random))) + 1):(min(((i*round((1/5) * nrow(breathing_random)))),nrow(breathing_random)))
  training <- breathing_random[-indices,]
  test <- breathing_random[indices,]
  #ans <- gammaImpute(formula(paste("Surv(breathing_whenevent,breathing_isevent==1)","~",paste(names(training)[c(3:15)],collapse="+"))),data=training,m=2 , gamma.factor=2,DCO.time=max(training$breathing_whenevent))
  #b=ExtractSingle(ans,index=1)
  #b=b$data
  #training=subset(b,select=-c(1:15,internalDCO.time,internal_gamma_val))
  #names(training)[c(length(names(training))-1,length(names(training)))]=c("when","is")
  #model=coxph(Surv(when,is==1)~.,data=training)
  print(i)
  
  model=rfsrc(Surv(breathing_whenevent,breathing_isevent==1)~.,data=training,na.action="na.impute",nimpute=3)
  v4[i]=survConcordance(Surv(breathing_whenevent,breathing_isevent)~predict(model,test,na.action="na.impute")$predicted,test)$concordance
  
}
death_random <- death_final_sub[sample(1:nrow(death_final_sub)),]
for (i in 1:5){
  indices <- (((i-1) * round((1/5)*nrow(death_random))) + 1):(min(((i*round((1/5) * nrow(death_random)))),nrow(death_random)))
  training <- death_random[-indices,]
  test <- death_random[indices,]
  #ans <- gammaImpute(formula(paste("Surv(when,is==1)","~",paste(names(training)[c(3:15)],collapse="+"))),data=training,m=2 , gamma.factor=2,DCO.time=max(training$when))
  #b=ExtractSingle(ans,index=1)
  #b=b$data
  #training=subset(b,select=-c(1:15,internalDCO.time,internal_gamma_val))
  #names(training)[c(length(names(training))-1,length(names(training)))]=c("when","is")
  #model=coxph(Surv(when,is==1)~.,data=training)
  print(i)
  model=rfsrc(Surv(when,is==1)~.,data=training,na.action="na.impute",nimpute=3)
  v5[i]=survConcordance(Surv(when,is)~predict(model,test,na.action="na.impute")$predicted,test)$concordance
  
}

cvplot(v1,v2,v3,v4,v5)
