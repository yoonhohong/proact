library(dplyr)
library(survival)
library(survminer)
# Read mitos staging data (See Preprocess1.R and Preprocess2.R)

setwd("~/Documents/ALSmaster_data")

mitos = read.csv("ALSFRS_MITOS_all.csv")
mitos_sub = read.csv("MITOS_excl_leftcensored.csv")

data.surv_training<-read.delim("surv_response_PROACT_training.txt",sep="|", header=T) 
data.surv_training2<-read.delim("surv_response_PROACT_training2.txt",sep="|", header=T) 
data.surv_leaderboard<-read.delim("surv_response_PROACT_leader.txt",sep="|", header=T) 
data.surv_validation<-read.delim("surv_response_PROACT_validation.txt",sep="|", header=T) 
survival <- rbind(data.surv_training,data.surv_training2,data.surv_leaderboard,data.surv_validation)
write.csv(survival, "Survival_all.csv", quote=F, row.names = F)

# Movement 생존분석 table : movement_tab가 최종
mitos %>%
  filter(Movement == TRUE) %>%
  group_by(SubjectID) %>%
  summarise(movement_delta = min(feature_delta)) -> temp1
temp1$lossMovement = T
mitos %>% 
  filter(!(SubjectID %in% temp1$SubjectID)) %>%
  group_by(SubjectID) %>%
  summarise(movement_delta = max(feature_delta)) -> temp2
temp2$lossMovement = F
movement_tab = rbind(temp1, temp2)
write.csv(movement_tab, "Movement_all.csv", quote = F, row.names = F)

## K-M plot
movement.survfit=survfit(Surv(movement_delta,lossMovement==1)~1,data=movement_tab)
plot(movement.survfit,xlab="Time",ylab="1 - Probability of LOF",main="Movement")

# Swallowing: swallowing_tab
mitos %>%
  filter(Swallowing == TRUE) %>%
  group_by(SubjectID) %>%
  summarise(swallowing_delta = min(feature_delta)) -> temp1
temp1$lossSwallowing = T
mitos %>% 
  filter(!(SubjectID %in% temp1$SubjectID)) %>%
  group_by(SubjectID) %>%
  summarise(swallowing_delta = max(feature_delta)) -> temp2
temp2$lossSwallowing = F
swallowing_tab = rbind(temp1, temp2)
write.csv(swallowing_tab, "Swallowing_all.csv", quote = F, row.names = F)

## K-M plot
swallowing.survfit=survfit(Surv(swallowing_delta,lossSwallowing==1)~1,data=swallowing_tab)
plot(swallowing.survfit,xlab="Time",ylab="1 - Probability of LOF",main="Swallowing")

# Communicating: communicating_tab
mitos %>%
  filter(Communicating == TRUE) %>%
  group_by(SubjectID) %>%
  summarise(communicating_delta = min(feature_delta)) -> temp1
temp1$lossCommunicating = T
mitos %>% 
  filter(!(SubjectID %in% temp1$SubjectID)) %>%
  group_by(SubjectID) %>%
  summarise(communicating_delta = max(feature_delta)) -> temp2
temp2$lossCommunicating = F
communicating_tab = rbind(temp1, temp2)
write.csv(communicating_tab, "Communicating_all.csv", quote = F, row.names = F)

## K-M plot
communicating.survfit=survfit(Surv(communicating_delta,lossCommunicating==1)~1,data=communicating_tab)
plot(communicating.survfit,xlab="Time",ylab="1 - Probability of LOF",main="Communicating")

# Breathing: breathing_tab
mitos %>%
  filter(Breathing == TRUE) %>%
  group_by(SubjectID) %>%
  summarise(breathing_delta = min(feature_delta)) -> temp1
temp1$lossBreathing = T
mitos %>% 
  filter(!(SubjectID %in% temp1$SubjectID)) %>%
  group_by(SubjectID) %>%
  summarise(breathing_delta = max(feature_delta)) -> temp2
temp2$lossBreathing = F
breathing_tab = rbind(temp1, temp2)
write.csv(breathing_tab, "Breathing_all.csv", quote = F, row.names = F)

## K-M plot
breathing.survfit=survfit(Surv(breathing_delta,lossBreathing==1)~1,data=breathing_tab)
plot(breathing.survfit,xlab="Time",ylab="1 - Probability of LOF",main="Breathing")

# death : breathing_survival가 최종

# names(survival) <- c("SubjectID","death_delta","death")
survival_sub <- survival[survival$SubjectID %in% mitos$SubjectID,]
survival.survfit=survfit(Surv(time_event,status==1)~1,data=survival_sub)
plot(survival.survfit,xlab="Time",ylab="Proportion surviving")

ggsurvplot(
  survival.survfit, 
  data = survival_sub, 
  risk.table = T,
  conf.int = T,
  xlim = c(0,1800),
  xlab = "Time in days",
  break.time.by = 300,
  ggtheme = theme_bw(),
  risk.table.y.text = FALSE
  )

# survival
plot(survival.survfit,xlab="Time",col="red",ylab="Probability",conf.int=FALSE, xlim = c(0,2114))
# all plot except survival
plot(movement.survfit,col="red",conf.int=FALSE)
lines(swallowing.survfit,col="blue",conf.int=FALSE)
lines(communicating.survfit,col=6,conf.int=FALSE)
lines(breathing.survfit,col="black",conf.int=FALSE)
legend("topright",c("Movement","Swallowing","Communicating", 
                    "Breathing"),lwd=2,bty="n",
       col=c("red","blue",6, "black"))
