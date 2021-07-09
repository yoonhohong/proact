
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
length(unique(slope_all$SubjectID)) # 3096 patients 
slope_all$SubjectID <- as.character(slope_all$SubjectID)

write.csv(slope_all, "ALSFRS_slope.csv", row.names = F, quote = F)

