library(dplyr)

setwd("~/Dropbox/Github/proact")

data.surv_training<-read.delim("~/Dropbox/ALSmaster/PROACT_data/PROACT_rawdata/surv_response_PROACT_training.txt",sep="|", header=T) 
data.surv_training2<-read.delim("~/Dropbox/ALSmaster/PROACT_data/PROACT_rawdata/surv_response_PROACT_training2.txt",sep="|", header=T) 
data.surv_leaderboard<-read.delim("~/Dropbox/ALSmaster/PROACT_data/PROACT_rawdata/surv_response_PROACT_leader.txt",sep="|", header=T) 
data.surv_validation<-read.delim("~/Dropbox/ALSmaster/PROACT_data/PROACT_rawdata/surv_response_PROACT_validation.txt",sep="|", header=T) 

survival <- rbind(data.surv_training,data.surv_training2,data.surv_leaderboard,data.surv_validation)

# unit conversion: time_event, from days to months 
survival$time_event = round(((survival$time_event)/365)*12, 2)

# check exact duplicates 
survival[duplicated(survival),] # no duplicates 
# check inconsistency 
survival %>%
  count(SubjectID, time_event) %>%
  filter(n>1) # no inconsistent data 

# report number of subjects 
length(unique(survival$SubjectID)) # 9080 patients 

write.csv(survival, "~/Dropbox/ALSmaster/PROACT_data/PROACT_preprocessed/survival.csv", quote=F, row.names = F)

