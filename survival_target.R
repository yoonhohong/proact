library(dplyr)

setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

data.surv_training<-read.delim("surv_response_PROACT_training.txt",sep="|", header=T) 
data.surv_training2<-read.delim("surv_response_PROACT_training2.txt",sep="|", header=T) 
data.surv_leaderboard<-read.delim("surv_response_PROACT_leader.txt",sep="|", header=T) 
data.surv_validation<-read.delim("surv_response_PROACT_validation.txt",sep="|", header=T) 
survival <- rbind(data.surv_training,data.surv_training2,data.surv_leaderboard,data.surv_validation)

# unit conversion: time_event, from days to mondths 
survival$time_event = round(((survival$time_event)/365)*12, 2)

# check exact duplicates 
survival[duplicated(survival),] # no duplicates 

# check inconsistency 
survival %>%
  count(SubjectID, time_event) %>%
  filter(n>1) # no inconsistent data 

# report number of subjects 
length(unique(survival$SubjectID)) # 9080 patients 

write.csv(survival, "survival.csv", quote=F, row.names = F)
