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

movement_merge3 <- merge(movement_survival_sub,allpatientsfeature_imputed_scaled,all.x=TRUE)
movement_final_sub<- subset(movement_merge3, select=-SubjectID)
swallowing_merge3 <- merge(swallowing_survival_sub,allpatientsfeature_imputed_scaled,all.x=TRUE)
swallowing_final_sub<- subset(swallowing_merge3, select=-SubjectID)
communicating_merge3 <- merge(communicating_survival_sub,allpatientsfeature_imputed_scaled,all.x=TRUE)
communicating_final_sub<- subset(communicating_merge3, select=-SubjectID)
breathing_merge3 <- merge(breathing_survival_sub,allpatientsfeature_imputed_scaled,all.x=TRUE)
breathing_final_sub<- subset(breathing_merge3, select=-SubjectID)
death_merge3 <- merge(death_survival_sub,allpatientsfeature_imputed_scaled,all.x=TRUE)
death_final_sub<- subset(death_merge3, select=-SubjectID)


# 각 domain에서 성능 plot 함수
cvplot <- function(x1,x2,x3,x4,x5){
 bbb=data.frame("Domain"=factor(c("Movement","Swallowing","Communicating","Breathing","Death"),levels=c("Death","Breathing","Communicating","Swallowing","Movement")),
                 "mean"=c(mean(x1),mean(x2),mean(x3),mean(x4),mean(x5)),
                 "inf"=c(mean(x1)-2.262*sd(x1)/sqrt(length(x1)),mean(x2)-2.262*sd(x2)/sqrt(length(x2)),mean(x3)-2.262*sd(x3)/sqrt(length(x3)),mean(x4)-2.262*sd(x4)/sqrt(length(x4)),mean(x5)-2.262*sd(x5)/sqrt(length(x5))),
                 "sup"=c(mean(x1)+2.262*sd(x1)/sqrt(length(x1)),mean(x2)+2.262*sd(x2)/sqrt(length(x2)),mean(x3)+2.262*sd(x3)/sqrt(length(x3)),mean(x4)+2.262*sd(x4)/sqrt(length(x4)),mean(x5)+2.262*sd(x5)/sqrt(length(x5))))
 ggplot(bbb,aes(x=mean,y=Domain))+geom_errorbarh(bbb, mapping=aes(y=Domain, xmin=inf, xmax=sup),height=0.2)+geom_point(bbb,mapping=aes(y=Domain, x=mean),size=2)+geom_text(bbb,mapping=aes(y=as.numeric(Domain)+0.3,x=mean,label=round(mean,2)))+ theme(axis.text=element_text(face="bold"),axis.title=element_blank())+scale_y_discrete(expand=c(0.5,0))
  # 2.776, 2.262
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
  print(i)
  model=coxph(Surv(movement_whenevent,movement_isevent==1)~.,data=training)
  v1[i]=survConcordance(Surv(movement_whenevent,movement_isevent)~predict(model,test),test)$concordance
}

swallowing_random <- swallowing_final_sub[sample(1:nrow(swallowing_final_sub)),]
for (i in 1:5){
  indices <- (((i-1) * round((1/5)*nrow(swallowing_random))) + 1):(min(((i*round((1/5) * nrow(swallowing_random)))),nrow(swallowing_random)))
  training <- swallowing_random[-indices,]
  test <- swallowing_random[indices,]
  print(i)
  model=coxph(Surv(swallowing_whenevent,swallowing_isevent==1)~.,data=training)
  v2[i]=survConcordance(Surv(swallowing_whenevent,swallowing_isevent)~predict(model,test),test)$concordance
  
}
communicating_random <- communicating_final_sub[sample(1:nrow(communicating_final_sub)),]
for (i in 1:5){
  indices <- (((i-1) * round((1/5)*nrow(communicating_random))) + 1):(min(((i*round((1/5) * nrow(communicating_random)))),nrow(communicating_random)))
  training <- communicating_random[-indices,]
  test <- communicating_random[indices,]
  print(i)
  model=coxph(Surv(communicating_whenevent,communicating_isevent==1)~.,data=training)
  v3[i]=survConcordance(Surv(communicating_whenevent,communicating_isevent)~predict(model,test),test)$concordance
  
}
breathing_random <- breathing_final_sub[sample(1:nrow(breathing_final_sub)),]
for (i in 1:5){
  indices <- (((i-1) * round((1/5)*nrow(breathing_random))) + 1):(min(((i*round((1/5) * nrow(breathing_random)))),nrow(breathing_random)))
  training <- breathing_random[-indices,]
  test <- breathing_random[indices,]
  print(i)
  model=coxph(Surv(breathing_whenevent,breathing_isevent==1)~.,data=training)
  v4[i]=survConcordance(Surv(breathing_whenevent,breathing_isevent)~predict(model,test),test)$concordance
  
}
death_random <- death_final_sub[sample(1:nrow(death_final_sub)),]
for (i in 1:5){
  indices <- (((i-1) * round((1/5)*nrow(death_random))) + 1):(min(((i*round((1/5) * nrow(death_random)))),nrow(death_random)))
  training <- death_random[-indices,]
  test <- death_random[indices,]
  print(i)
  model=coxph(Surv(when,is==1)~.,data=training)
  v5[i]=survConcordance(Surv(when,is)~predict(model,test),test)$concordance
  
}
cvplot(v1,v2,v3,v4,v5)
