# Preprocess data for staging 

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# Load packages
library(tidyverse)
library(survival)
library(broom)
library(purrr)

alsfrs = read.csv("PROACT_preprocessed/ALSFRS_rev.csv")


# Merge datasets 
merged_rev = demographic %>%
  inner_join(alshx, by = "SubjectID") %>% 
  inner_join(alsfrs_rev_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(fvc_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(famhx, by = "SubjectID") %>%
  left_join(riluzole, by = "SubjectID") %>%
  left_join(bmi, by = "SubjectID") %>%
  left_join(vitalsign_3mo_aggr, by = "SubjectID") %>%
  left_join(lab_3mo_aggr, by = "SubjectID")

write.csv(merged_rev, "PROACT_preprocessed_rev.csv", quote = F, row.names = F)

# The End # 

