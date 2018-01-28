# Variable importance plot
movement_cox1=rfsrc(Surv(movement_whenevent,movement_isevent==1)~.,data=movement_final_sub,na.action="na.impute",nimpute=3,importance=TRUE)
swallowing_cox1=rfsrc(Surv(swallowing_whenevent,swallowing_isevent==1)~.,data=swallowing_final_sub,na.action="na.impute",nimpute=3,importance=TRUE)
communicating_cox1=rfsrc(Surv(communicating_whenevent,communicating_isevent==1)~.,data=communicating_final_sub,na.action="na.impute",nimpute=3,importance=TRUE)
breathing_cox1=rfsrc(Surv(breathing_whenevent,breathing_isevent==1)~.,data=breathing_final_sub,na.action="na.impute",nimpute=3,importance=TRUE)
death_cox1=rfsrc(Surv(when,is==1)~.,data=death_final_sub,na.action="na.impute",nimpute=3,importance=TRUE)


par(las=2)
par(mar=c(4,8,1,1))
barplot(movement_cox1$importance[order(movement_cox1$importance)][c((length(movement_cox1$importance)-15):(length(movement_cox1$importance)))],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Movement")

par(las=2)
par(mar=c(4,8,1,1))
barplot(swallowing_cox1$importance[order(swallowing_cox1$importance)][c((length(swallowing_cox1$importance)-15):(length(swallowing_cox1$importance)))],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Swallowing")

par(las=2)
par(mar=c(4,8,1,1))
barplot(communicating_cox1$importance[order(communicating_cox1$importance)][c((length(communicating_cox1$importance)-15):(length(communicating_cox1$importance)))],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Communicating")

par(las=2)
par(mar=c(4,8,1,1.3))
barplot(breathing_cox1$importance[order(breathing_cox1$importance)][c((length(breathing_cox1$importance)-15):(length(breathing_cox1$importance)))],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Breathing")

par(las=2)
par(mar=c(4,8,1,1))
barplot(death_cox1$importance[order(death_cox1$importance)][c((length(death_cox1$importance)-15):(length(death_cox1$importance)))],hori=TRUE,las=1,cex.names=1,main="Variable Importance : Death")


# Examples of predicted curve
movement_cox=coxph(Surv(movement_whenevent,movement_isevent==1)~.,data=movement_final_sub)
swallowing_cox=coxph(Surv(swallowing_whenevent,swallowing_isevent==1)~.,data=swallowing_final_sub)
communicating_cox=coxph(Surv(communicating_whenevent,communicating_isevent==1)~.,data=communicating_final_sub)
breathing_cox=coxph(Surv(breathing_whenevent,breathing_isevent==1)~.,data=breathing_final_sub)
death_cox=coxph(Surv(when,is==1)~.,data=death_final_sub)

movement_pred=predict(movement_cox1,allpatientsfeature_nonimputed,na.action="na.impute")
swallowing_pred=predict(swallowing_cox1,allpatientsfeature_nonimputed,na.action="na.impute")
communicating_pred=predict(communicating_cox1,allpatientsfeature_nonimputed,na.action="na.impute")
breathing_pred=predict(breathing_cox1,allpatientsfeature_nonimputed,na.action="na.impute")
death_pred=predict(death_cox1,allpatientsfeature_nonimputed,na.action="na.impute")

i=500
plot(movement_pred$time.interest,movement_pred$survival[i,],xlab="Time",ylab="Probability of non-loss",type="l",col=1,ylim=c(0,1))
lines(swallowing_pred$time.interest,swallowing_pred$survival[i,],type="l",col=2)
lines(communicating_pred$time.interest,communicating_pred$survival[i,],type="l",col=3)
lines(breathing_pred$time.interest,breathing_pred$survival[i,],type="l",col=4)
lines(death_pred$time.interest,death_pred$survival[i,],type="l",col=6)
legend("topright",c("Movement","Swallowing","Communicating","Breathing","Death"),lwd=2,bty="n",col=c(1,2,3,4,6))
i=1023
plot(survfit(breathing_cox,allpatientsfeature_imputed_scaled[i,]),xlab="Time",ylab="Probability of non-loss",conf.int=FALSE)
lines(survfit(movement_cox,allpatientsfeature_imputed_scaled[i,]),col="green",conf.int=FALSE)
lines(survfit(swallowing_cox,allpatientsfeature_imputed_scaled[i,]),col="red",conf.int=FALSE)
lines(survfit(communicating_cox,allpatientsfeature_imputed_scaled[i,]),col="blue",conf.int=FALSE)
lines(survfit(death_cox,allpatientsfeature_imputed_scaled[i,]),col=6,conf.int=FALSE)
legend("topright",c("Movement","Swallowing","Communicating","Breathing","Death"),lwd=2,bty="n",col=c("green","red","blue","black",6))
allpatientsfeature_nonimputed[i,]
