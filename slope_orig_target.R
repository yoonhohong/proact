
# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# Load packages
library(dplyr)
library(tidyr)

# ALSFRS_slope datasets 
slope_training = read.delim("ALSFRS_slope_PROACT_training.txt", sep = "|", header = T)
slope_training2 = read.delim("ALSFRS_slope_PROACT_training2.txt", sep = "|", header = T)
slope_validation = read.delim("ALSFRS_slope_PROACT_validation.txt", sep = "|", header = T)
slope_leaderboard = read.delim("ALSFRS_slope_PROACT_leaderboard.txt", sep = "|", header = T)
slope_all = rbind(slope_training, slope_training2, slope_leaderboard, slope_validation)

# check duplicates 
slope_all[duplicated(slope_all),]

# report the number of subjects 
length(unique(slope_all$SubjectID)) # 3096 patients 

write.csv(slope_all, "ALSFRS_slope_target.csv", row.names = F, quote = F)



