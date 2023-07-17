# Preprocess data for staging 

# Set working directory 
setwd("~/Dropbox/Github/proact")

# Load packages
library(tidyverse)

proact = read.csv("~/Dropbox/ALSmaster/PROACT_data/PROACT_preprocessed/PROACT_features_predproj.csv")

feat_select = proact %>%
  select(SubjectID, Age, Gender, onset_delta, onset_site)
dim(feat_select)

write.csv(feat_select, "~/Dropbox/ALSmaster/PROACT_data/PROACT_preprocessed/PROACT_features_stage.csv", quote = F, row.names = F)

# The End # 

