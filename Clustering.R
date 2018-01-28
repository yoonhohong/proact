movement_prob=movement_pred$survival[,sindex(movement_pred$time.interest,30.5*12)]
swallowing_prob=swallowing_pred$survival[,sindex(swallowing_pred$time.interest,30.5*12)]
communicating_prob=communicating_pred$survival[,sindex(communicating_pred$time.interest,30.5*12)]
breathing_prob=breathing_pred$survival[,sindex(breathing_pred$time.interest,30.5*12)]
death_prob=death_pred$survival[,sindex(death_pred$time.interest,30.5*12)]

movement_curve=survfit(movement_cox1,newdata=allpatientsfeature_imputed)
movement_prob=movement_curve$surv[sindex(movement_curve$time,30.5 * 12),]
swallowing_curve=survfit(swallowing_cox1,newdata=allpatientsfeature_imputed)
swallowing_prob=swallowing_curve$surv[sindex(swallowing_curve$time,30.5 * 12),]
communicating_curve=survfit(communicating_cox1,newdata=allpatientsfeature_imputed)
communicating_prob=communicating_curve$surv[sindex(communicating_curve$time,30.5 * 12),]
breathing_curve=survfit(breathing_cox1,newdata=allpatientsfeature_imputed)
breathing_prob=breathing_curve$surv[sindex(breathing_curve$time,30.5 * 12),]
death_curve=survfit(death_cox1,newdata=allpatientsfeature_imputed)
death_prob=death_curve$surv[sindex(death_curve$time,30.5 * 12),]


movement_prob=1-movement_prob
swallowing_prob=1-swallowing_prob
communicating_prob=1-communicating_prob
breathing_prob=1-breathing_prob
death_prob=1-death_prob

movement_prob_scaled=scale(movement_prob)
swallowing_prob_scaled=scale(swallowing_prob)
communicating_prob_scaled=scale(communicating_prob)
breathing_prob_scaled=scale(breathing_prob)
death_prob_scaled=scale(death_prob)

allprob=data.frame("SubjectID"=allpatientsfeature_nonimputed$SubjectID,movement_prob,swallowing_prob,communicating_prob,breathing_prob,death_prob)
allprob_scaled=data.frame("SubjectID"=allpatientsfeature_imputed$SubjectID,movement_prob_scaled,swallowing_prob_scaled,communicating_prob_scaled,breathing_prob_scaled,death_prob_scaled)


wss<-0
for ( i in 1:15){
  km.out <- kmeans(scale(allprob[,-1]),centers=i,nstart=20)
  wss[i] <- km.out$tot.withinss
}
plot(1:15, wss, type="b")
km.out <- kmeans(allprob[,-1],centers=5,nstart=20)

wss_scaled<-0
for ( i in 1:15){
  km.out_scaled <- kmeans(allprob_scaled[,-1],centers=i,nstart=20)
  wss_scaled[i] <- km.out_scaled$tot.withinss
}
plot(1:15, wss_scaled, type="b")
km.out_scaled <- kmeans(allprob_scaled[,-1],centers=3,nstart=20)
table(km.out$cluster,km.out_scaled$cluster)


allcluster <- data.frame(allpatientsfeature_nonimputed,allprob[,-1],km.out$cluster)
allcluster$km.out.cluster <- factor(allcluster$km.out.cluster)
plot(allcluster$onset_site~allcluster$km.out.cluster)
plot(allcluster$km.out.cluster~allcluster$onset_site)
barplot(c("movement"=mean(allcluster[allcluster$km.out.cluster==1,]$movement_prob),"swallowing"=mean(allcluster[allcluster$km.out.cluster==1,]$swallowing_prob),"communi"=mean(allcluster[allcluster$km.out.cluster==1,]$communicating_prob),"breathing"=mean(allcluster[allcluster$km.out.cluster==1,]$breathing_prob),"death"=mean(allcluster[allcluster$km.out.cluster==1,]$death_prob)),ylim=c(0,1),main="Cluster 1")
barplot(c("movement"=mean(allcluster[allcluster$km.out.cluster==2,]$movement_prob),"swallowing"=mean(allcluster[allcluster$km.out.cluster==2,]$swallowing_prob),"communi"=mean(allcluster[allcluster$km.out.cluster==2,]$communicating_prob),"breathing"=mean(allcluster[allcluster$km.out.cluster==2,]$breathing_prob),"death"=mean(allcluster[allcluster$km.out.cluster==2,]$death_prob)),ylim=c(0,1),main="Cluster 2")
barplot(c("movement"=mean(allcluster[allcluster$km.out.cluster==3,]$movement_prob),"swallowing"=mean(allcluster[allcluster$km.out.cluster==3,]$swallowing_prob),"communi"=mean(allcluster[allcluster$km.out.cluster==3,]$communicating_prob),"breathing"=mean(allcluster[allcluster$km.out.cluster==3,]$breathing_prob),"death"=mean(allcluster[allcluster$km.out.cluster==3,]$death_prob)),ylim=c(0,1),main="Cluster 3")

boxplot(allcluster$movement_prob~allcluster$km.out.cluster,outline=FALSE)
boxplot(allcluster$swallowing_prob~allcluster$km.out.cluster,outline=FALSE)
boxplot(allcluster$communicating_prob~allcluster$km.out.cluster,outline=FALSE)
boxplot(allcluster$breathing_prob~allcluster$km.out.cluster,outline=FALSE)
boxplot(allcluster$death_prob~allcluster$km.out.cluster,outline=FALSE)



hclust.out <- hclust(dist(scale(allprob[,-1])))
plot(hclust.out)
hclust=cutree(hclust.out,k=4)
table(km.out$cluster,hclust)
allclusterbyh <- data.frame(allpatientsfeature_nonimputed,allprob[,-1],hclust)
allclusterbyh$hclust <- factor(allclusterbyh$hclust)

plot(allclusterbyh$onset_site~allclusterbyh$hclust)

boxplot(allclusterbyh$movement_prob~allclusterbyh$hclust,outline=FALSE)
boxplot(allclusterbyh$swallowing_prob~allclusterbyh$hclust,outline=FALSE)
boxplot(allclusterbyh$communicating_prob~allclusterbyh$hclust,outline=FALSE)
boxplot(allclusterbyh$breathing_prob~allclusterbyh$hclust,outline=FALSE)
boxplot(allclusterbyh$death_prob~allclusterbyh$hclust,outline=FALSE)

barplot(c("movement"=mean(allclusterbyh[allclusterbyh$hclust==1,]$movement_prob),"swallowing"=mean(allclusterbyh[allclusterbyh$hclust==1,]$swallowing_prob),"communi"=mean(allclusterbyh[allclusterbyh$hclust==1,]$communicating_prob),"breathing"=mean(allclusterbyh[allclusterbyh$hclust==1,]$breathing_prob),"death"=mean(allclusterbyh[allclusterbyh$hclust==1,]$death_prob)),ylim=c(0,1),main="Cluster 1")
barplot(c("movement"=mean(allclusterbyh[allclusterbyh$hclust==2,]$movement_prob),"swallowing"=mean(allclusterbyh[allclusterbyh$hclust==2,]$swallowing_prob),"communi"=mean(allclusterbyh[allclusterbyh$hclust==2,]$communicating_prob),"breathing"=mean(allclusterbyh[allclusterbyh$hclust==2,]$breathing_prob),"death"=mean(allclusterbyh[allclusterbyh$hclust==2,]$death_prob)),ylim=c(0,1),main="Cluster 2")
barplot(c("movement"=mean(allclusterbyh[allclusterbyh$hclust==3,]$movement_prob),"swallowing"=mean(allclusterbyh[allclusterbyh$hclust==3,]$swallowing_prob),"communi"=mean(allclusterbyh[allclusterbyh$hclust==3,]$communicating_prob),"breathing"=mean(allclusterbyh[allclusterbyh$hclust==3,]$breathing_prob),"death"=mean(allclusterbyh[allclusterbyh$hclust==3,]$death_prob)),ylim=c(0,1),main="Cluster 3")
barplot(c("movement"=mean(allclusterbyh[allclusterbyh$hclust==4,]$movement_prob),"swallowing"=mean(allclusterbyh[allclusterbyh$hclust==4,]$swallowing_prob),"communi"=mean(allclusterbyh[allclusterbyh$hclust==4,]$communicating_prob),"breathing"=mean(allclusterbyh[allclusterbyh$hclust==4,]$breathing_prob),"death"=mean(allclusterbyh[allclusterbyh$hclust==4,]$death_prob)),ylim=c(0,1),main="Cluster 4")


