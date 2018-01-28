library(dplyr)
library(tidyr)
library(survival)

# 데이터 read
data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.allforms <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)

data.surv_training<-read.delim("surv_response_PROACT_training.txt",sep="|", header=T) 
data.surv_training2<-read.delim("surv_response_PROACT_training2.txt",sep="|", header=T) 
data.surv_leaderboard<-read.delim("surv_response_PROACT_leader.txt",sep="|", header=T) 
data.surv_validation<-read.delim("surv_response_PROACT_validation.txt",sep="|", header=T) 
death_survival <- rbind(data.surv_training,data.surv_training2,data.surv_leaderboard,data.surv_validation)
death_survival$status <- as.logical(death_survival$status)


# ALSFRS form만 불러오기
data.alsfrs1 <- droplevels(data.allforms[data.allforms$form_name=="ALSFRS", ])
data.alsfrs1 = subset(data.alsfrs1,select=-c(form_name,feature_unit))
data.alsfrs1$feature_value <- as.numeric(as.character(data.alsfrs1$feature_value))
data.alsfrs1$feature_delta <- as.numeric(as.character(data.alsfrs1$feature_delta))


# ALSFRS에서 같은 환자가 같은 시점에 2번 측정한 데이터 처리 by 평균 
data.alsfrs1 <- subset(data.alsfrs1,!(SubjectID==137943 & feature_delta==371))
a <- mutate(group_by(data.alsfrs1,SubjectID,feature_name,feature_delta),n=n())
b=subset(a,n>1)
c=subset(a,n==1)
c=subset(c,select=-n)
b <- summarize(b,feature_value=mean(feature_value,na.rm=TRUE))
c <- as.data.frame(c)
b <- as.data.frame(b)
d <- rbind(c,b)

# spread 형태로 변환
alsfrsfull <- spread(d,feature_name,feature_value)

# feature_delta가 missing이거나 각 item 중 하나라도 missing이 있는 데이터 제외
alsfrsfull <- droplevels(filter(alsfrsfull, !is.na(feature_delta)))
alsfrsfull <- droplevels(filter(alsfrsfull,!is.na(Q1_Speech)&!is.na(Q2_Salivation)&!is.na(Q3_Swallowing)&!is.na(Q4_Handwriting)&(!(is.na(Q5a_Cutting_without_Gastrostomy)&is.na(Q5b_Cutting_with_Gastrostomy)))&!is.na(Q6_Dressing_and_Hygiene)&!is.na(Q7_Turning_in_Bed)&!is.na(Q8_Walking)&!is.na(Q9_Climbing_Stairs)&!(is.na(Q10_Respiratory)&(is.na(R1_Dyspnea)|is.na(R2_Orthopnea)|is.na(R3_Respiratory_Insufficiency)))))

# alsfrs인지 alsfrs-r인지 구별하는 변수 추가
alsfrsfull$if_R <- !is.na(alsfrsfull$R1_Dyspnea) & !is.na(alsfrsfull$R3_Respiratory_Insufficiency)

# Q10과 R1,R2,R3을 적절하게 고려하여 Q10R 점수로 변환
# Q10R에 따라 ALSFRS_Total도 ALSFRS_TotalR로 변환
alsfrsfull$Q10R1=ifelse(alsfrsfull$R1_Dyspnea==4,4,ifelse(alsfrsfull$R1_Dyspnea>1,3,ifelse(alsfrsfull$R1_Dyspnea>0,2,1)))
alsfrsfull$Q10R3=ifelse(alsfrsfull$R3_Respiratory_Insufficiency==2,ifelse(alsfrsfull$R1_Dyspnea>1,2,1),ifelse(alsfrsfull$R3_Respiratory_Insufficiency<2 & alsfrsfull$R3_Respiratory_Insufficiency>0,1,ifelse(alsfrsfull$R3_Respiratory_Insufficiency==0,0,NA)))
alsfrsfull$Q10R3[is.na(alsfrsfull$Q10R3)]=alsfrsfull$Q10R1[is.na(alsfrsfull$Q10R3)]  
alsfrsfull$Q10R3[is.na(alsfrsfull$Q10R3)]=alsfrsfull$Q10_Respiratory[is.na(alsfrsfull$Q10R3)]
alsfrsfull$Q10R=alsfrsfull$Q10R3
alsfrsfull <- subset(alsfrsfull,select=-c(Q10R1,Q10R3))
alsfrsfull$ALSFRS_TotalR = alsfrsfull$ALSFRS_Total - alsfrsfull$respiratory + alsfrsfull$Q10R


# King's stage 추가
alsfrsfull$kingbulbar <- (alsfrsfull$Q1_Speech<4) | (alsfrsfull$Q2_Salivation<4) | (alsfrsfull$Q3_Swallowing<4) 
alsfrsfull$kingulimb[is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)] <- (alsfrsfull$Q4_Handwriting[is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)]<4)
alsfrsfull$kingulimb[!is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)] <- (alsfrsfull$Q4_Handwriting[!is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)]<4) | (alsfrsfull$Q5a_Cutting_without_Gastrostomy[!is.na(alsfrsfull$Q5a_Cutting_without_Gastrostomy)]<4)
alsfrsfull$kingllimb <- (alsfrsfull$Q8_Walking<4)
alsfrsfull$kingnut <- !is.na(alsfrsfull$Q5b_Cutting_with_Gastrostomy)

## revise needed below
alsfrsfull$kingres <- (alsfrsfull$R1_Dyspnea==0) | (alsfrsfull$R3_Respiratory_Insufficiency<4)
alsfrsfull$kingres[is.na(alsfrsfull$kingres)] <- (alsfrsfull$Q10_Respiratory[is.na(alsfrsfull$kingres)] <=1 )

alsfrsfull$kings=ifelse(alsfrsfull$kingres==1 | alsfrsfull$kingnut==1,4,alsfrsfull$kingbulbar+alsfrsfull$kingllimb+alsfrsfull$kingulimb)


# 3점스케일 변환 후  mean of Q1~Q3/Q4~Q9/Q10~Q12 : 변수 3개 추가
alsfrsfull$temp1 <- ifelse(alsfrsfull$Q1_Speech==4,2,ifelse(alsfrsfull$Q1_Speech>=2,1,0))
alsfrsfull$temp2 <- ifelse(alsfrsfull$Q2_Salivation==4,2,ifelse(alsfrsfull$Q2_Salivation>=2,1,0))
alsfrsfull$temp3 <- ifelse(alsfrsfull$Q3_Swallowing==4,2,ifelse(alsfrsfull$Q3_Swallowing>=2,1,0))
alsfrsfull$temp4 <- ifelse(alsfrsfull$Q4_Handwriting==4,2,ifelse(alsfrsfull$Q4_Handwriting>=2,1,0))
alsfrsfull$temp5 <- ifelse(alsfrsfull$Q5_Cutting==4,2,ifelse(alsfrsfull$Q5_Cutting>=2,1,0))
alsfrsfull$temp6 <- ifelse(alsfrsfull$Q6_Dressing_and_Hygiene==4,2,ifelse(alsfrsfull$Q6_Dressing_and_Hygiene>=2,1,0))
alsfrsfull$temp7 <- ifelse(alsfrsfull$Q7_Turning_in_Bed==4,2,ifelse(alsfrsfull$Q7_Turning_in_Bed>=2,1,0))
alsfrsfull$temp8 <- ifelse(alsfrsfull$Q8_Walking==4,2,ifelse(alsfrsfull$Q8_Walking>=2,1,0))
alsfrsfull$temp9 <- ifelse(alsfrsfull$Q9_Climbing_Stairs==4,2,ifelse(alsfrsfull$Q9_Climbing_Stairs>=2,1,0))
alsfrsfull$temp10 <- ifelse(alsfrsfull$Q10_Respiratory==4,2,ifelse(alsfrsfull$Q10_Respiratory>=3,1,0))
alsfrsfull$temp11 <- ifelse(alsfrsfull$R1_Dyspnea==4,2,ifelse(alsfrsfull$R1_Dyspnea>=2,1,0))
alsfrsfull$temp12 <- ifelse(alsfrsfull$R2_Orthopnea==4,2,ifelse(alsfrsfull$R2_Orthopnea>=2,1,0))
alsfrsfull$temp13 <- ifelse(alsfrsfull$R3_Respiratory_Insufficiency==4,2,ifelse(alsfrsfull$R3_Respiratory_Insufficiency>=1,1,0))

alsfrsfull$multibulbar <- (alsfrsfull$temp1 + alsfrsfull$temp2 + alsfrsfull$temp3)/3
alsfrsfull$multimotor <- (alsfrsfull$temp4+alsfrsfull$temp5+alsfrsfull$temp6+alsfrsfull$temp7+alsfrsfull$temp8+alsfrsfull$temp9)/6
alsfrsfull$multirespi=alsfrsfull$temp10
alsfrsfull$multirespi[is.na(alsfrsfull$multirespi)]=alsfrsfull$temp11[is.na(alsfrsfull$multirespi)]
alsfrsfull <- subset(alsfrsfull,select=-c(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12,temp13))



# ALS MITOS 추가
Movement <- (alsfrsfull$Q8_Walking <=1 | alsfrsfull$Q6_Dressing_and_Hygiene<=1)
Swallowing <- (alsfrsfull$Q3_Swallowing <= 1 )
Communicating <- (alsfrsfull$Q1_Speech<=1 & alsfrsfull$Q4_Handwriting <=1)
Breathing <- (alsfrsfull$R1_Dyspnea<=1 | alsfrsfull$R3_Respiratory_Insufficiency<=2)
Breathing[is.na(Breathing)]=(alsfrsfull$Q10_Respiratory[is.na(Breathing)]<=2)
ALSMITOS <- Movement + Swallowing + Communicating + Breathing
alsfrsfull <- mutate(alsfrsfull, Movement,Swallowing,Communicating,Breathing,ALSMITOS)

# Movement 생존분석 table : movement_survival_sub가 최종
group<-group_by(alsfrsfull,SubjectID)
time_rank <- mutate(group,rank=rank(feature_delta))
time_rank <- as.data.frame(time_rank)
movement_isevent <- summarize(group,any(Movement==1))
movement_whenevent1 <- summarize(group_by(time_rank,SubjectID),ifelse(min(rank[which(Movement==1)])==1,-Inf, (1 *min(feature_delta[which(Movement==1)])+0*feature_delta[rank==min(rank[which(Movement==1)])-1])))
movement_whenevent2 <- summarize(group, max(feature_delta[which(Movement==0)]))
movement_whenevent <- vector()
for (i in 1:length(movement_isevent[[2]])){
  if (movement_isevent[[2]][i]==0) {
    movement_whenevent[i]=movement_whenevent2[[2]][i]
  }
  else if (movement_isevent[[2]][i]==1){
    movement_whenevent[i]=movement_whenevent1[[2]][i]
  }
}
movement_survival <- data.frame("SubjectID"=movement_isevent[[1]],movement_whenevent,"movement_isevent"=movement_isevent[[2]])
movement_survival=movement_survival[order(movement_survival$movement_whenevent),]

## left censoring 제외
movement_survival <- droplevels(subset(movement_survival,movement_whenevent!=-Inf))

## 92일 이전에 이벤트 발생하였거나 right censoring된 환자 제외
movement_survival_sub <-droplevels(filter(movement_survival,movement_whenevent>=92))

## K-M plot
movement.survfit=survfit(Surv(movement_whenevent,movement_isevent==1)~1,data=movement_survival_sub)
plot(movement.survfit,xlab="Time",ylab="Proportion non-loss of function",main="Movement")


# Swallowing : swallowing_survival_sub가 최종
group<-group_by(alsfrsfull,SubjectID)
time_rank <- mutate(group,rank=rank(feature_delta))
time_rank <- as.data.frame(time_rank)
swallowing_isevent <- summarize(group,any(Swallowing==1))
swallowing_whenevent1 <- summarize(group_by(time_rank,SubjectID),ifelse(min(rank[which(Swallowing==1)])==1,-Inf,(1 *min(feature_delta[which(Swallowing==1)])+0*feature_delta[rank==min(rank[which(Swallowing==1)])-1])))
swallowing_whenevent2 <- summarize(group, max(feature_delta[which(Swallowing==0)]))
swallowing_whenevent <- vector()
for (i in 1:length(swallowing_isevent[[2]])){
  if (swallowing_isevent[[2]][i]==0) {
    swallowing_whenevent[i]=swallowing_whenevent2[[2]][i]
  }
  else if (swallowing_isevent[[2]][i]==1){
    swallowing_whenevent[i]=swallowing_whenevent1[[2]][i]
  }
}
swallowing_survival <- data.frame("SubjectID"=swallowing_isevent[[1]],swallowing_whenevent,"swallowing_isevent"=swallowing_isevent[[2]])
swallowing_survival=swallowing_survival[order(swallowing_survival$swallowing_whenevent),]

swallowing_survival <- droplevels(subset(swallowing_survival,swallowing_whenevent!=-Inf))

swallowing_survival_sub <-droplevels(filter(swallowing_survival,swallowing_whenevent>=92))

swallowing.survfit=survfit(Surv(swallowing_whenevent,swallowing_isevent==1)~1,data=swallowing_survival_sub)
plot(swallowing.survfit,xlab="Time",ylab="Proportion non-loss of function",main="Swallowing")



# Communicating  : communicating_survival_sub가 최종
group<-group_by(alsfrsfull,SubjectID)
time_rank <- mutate(group,rank=rank(feature_delta))
time_rank <- as.data.frame(time_rank)
communicating_isevent <- summarize(group,any(Communicating==1))
communicating_whenevent1 <- summarize(group_by(time_rank,SubjectID), ifelse(min(rank[which(Communicating==1)])==1,-Inf,(1 *min(feature_delta[which(Communicating==1)])+0*feature_delta[rank==min(rank[which(Communicating==1)])-1])))
communicating_whenevent2 <- summarize(group, max(feature_delta[which(Communicating==0)]))
communicating_whenevent <- vector()
for (i in 1:length(communicating_isevent[[2]])){
  if (communicating_isevent[[2]][i]==0) {
    communicating_whenevent[i]=communicating_whenevent2[[2]][i]
  }
  else if (communicating_isevent[[2]][i]==1){
    communicating_whenevent[i]=communicating_whenevent1[[2]][i]
  }
}
communicating_survival <- data.frame("SubjectID"=communicating_isevent[[1]],communicating_whenevent,"communicating_isevent"=communicating_isevent[[2]])
communicating_survival=communicating_survival[order(communicating_survival$communicating_whenevent),]

communicating_survival <- droplevels(subset(communicating_survival,communicating_whenevent!=-Inf))

communicating_survival_sub <-droplevels(filter(communicating_survival,communicating_whenevent>=92))

communicating.survfit=survfit(Surv(communicating_whenevent,communicating_isevent==1)~1,data=communicating_survival_sub)
plot(communicating.survfit,xlab="Time",ylab="Proportion non-loss of function",main="Communicating")

# breathing : breathing_survival_sub가 최종
group<-group_by(alsfrsfull,SubjectID)
time_rank <- mutate(group,rank=rank(feature_delta))
time_rank <- as.data.frame(time_rank)
breathing_isevent <- summarize(group,any(Breathing==1))
breathing_whenevent1 <- summarize(group_by(time_rank,SubjectID), ifelse(min(rank[which(Breathing==1)])==1,-Inf,(1 *min(feature_delta[which(Breathing==1)])+0*feature_delta[rank==min(rank[which(Breathing==1)])-1])))
breathing_whenevent2 <- summarize(group, max(feature_delta[which(Breathing==0)]))
breathing_whenevent <- vector()
for (i in 1:length(breathing_isevent[[2]])){
  if (breathing_isevent[[2]][i]==0) {
    breathing_whenevent[i]=breathing_whenevent2[[2]][i]
  }
  else if (breathing_isevent[[2]][i]==1){
    breathing_whenevent[i]=breathing_whenevent1[[2]][i]
  }
}
breathing_survival <- data.frame("SubjectID"=breathing_isevent[[1]],breathing_whenevent,"breathing_isevent"=breathing_isevent[[2]])
breathing_survival=breathing_survival[order(breathing_survival$breathing_whenevent),]

breathing_survival <- droplevels(subset(breathing_survival,breathing_whenevent!=-Inf))

breathing_survival_sub <-droplevels(filter(breathing_survival,breathing_whenevent>=92))

breathing.survfit=survfit(Surv(breathing_whenevent,breathing_isevent==1)~1,data=breathing_survival_sub)
plot(breathing.survfit,xlab="Time",ylab="Proportion non-loss of function",main="Breathing")


# death : breathing_survival가 최종
names(death_survival) <- c("SubjectID","when","is")
death_survival_sub <-semi_join(death_survival,alsfrsfull)
death.survfit=survfit(Surv(when,is==1)~1,data=death_survival_sub)
plot(death.survfit,xlab="Time",ylab="Proportion surviving", main="Death")


# all plot
plot(breathing.survfit,xlab="Time",ylab="Proportion surviving",conf.int=FALSE)
lines(communicating.survfit,col="blue",conf.int=FALSE)
lines(swallowing.survfit,col="red",conf.int=FALSE)
lines(movement.survfit,col="green",conf.int=FALSE)
lines(death.survfit,col=6,conf.int=FALSE)
legend("topright",c("Movement","Swallowing","Communicating","Breathing","Death"),lwd=2,bty="n",col=c("green","red","blue","black",6))