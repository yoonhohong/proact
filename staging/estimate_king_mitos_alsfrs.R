# Estimate clinical stages (King's and MiTos) with ALS functional rating scale revised

# Reference

# Rubika Balendra (2014) Estimating clinical stage of amyotrophic lateral sclerosis from the ALS Functional Rating Scale, Amyotrophic Lateral Sclerosis and Frontotemporal Degeneration, 15:3-4, 279-284, DOI: 10.3109/21678421.2014.897357   

# Chiò A, Hammond ER, Mora G, et al. J Neurol Neurosurg Psychiatry 2015;86:38–44.  

library(tidyverse)
setwd("/Users/hong/Dropbox/ALSmaster/PROACT") 


# read-in ALSFRS_rev data
alsfrs <- read.csv("PROACT_preprocessed/ALSFRS_rev.csv")
names(alsfrs) = tolower(names(alsfrs))

# King's staging   
# Patients who have undergone gastrostomy or are on NIV are classified as stage 4, and those who are not stage 4 are assigned stage 1-3 based on the number of bulbar, upper, and lower extremities involved. 

stage_king = alsfrs %>%
  mutate(bulbar_involved = ifelse(bulbar < 12, 1, 0), 
         upperlimb_involved = ifelse((q4_handwriting + q5_cutting) < 8, 1, 0), 
         lowerlimb_involved = ifelse(q8_walking < 4, 1, 0)) %>%
  mutate(king = bulbar_involved + upperlimb_involved + lowerlimb_involved) %>%
  mutate(king = case_when(
    (gastrostomy == T)|(r1_dyspnea == 0|r3_respiratory_insufficiency < 4) ~ 4, 
    TRUE ~ king)) %>% 
  select(subjectid, feature_delta, king)

stage_king %>%
  count(king) %>%
  mutate(prop = round(n/sum(n)*100, 2))

# stage = 0 in 119 observations, why?  
# trunk (Q6, Q7) involved or 
# leg involved in climbing stairs (Q9) but not in walking (Q8) or.  
# respiration involved (Q10) to such an extent that does not require NIV   

# replace stage 0 with stage 1 
stage_king$king = ifelse(stage_king$king == 0, 1, stage_king$king)

stage_king = within(stage_king, {
  king = factor(king)
  subjectid = as.character(subjectid)
})
summary(stage_king)

stage_king %>%
  count(king) %>%
  mutate(prop = round(n/sum(n)*100, 1))


# MiToS staging

stage_mitos = alsfrs %>%
  mutate(swallowing = ifelse(q3_swallowing <= 1, 1, 0)) %>%
  mutate(breathing = ifelse(r1_dyspnea <= 1|r3_respiratory_insufficiency <= 2, 1, 0)) %>%
  mutate(communicating = ifelse(q1_speech <= 1 & 
                                  q4_handwriting <=1, 1, 0)) %>%
  mutate(movement = ifelse(q6_dressing_and_hygiene <= 1|
                             q8_walking <=1, 1, 0)) %>%
  mutate(mitos = swallowing + breathing + communicating + movement) %>%
  select(subjectid, feature_delta, mitos)

stage_mitos = within(stage_mitos, {
  subjectid = as.character(subjectid)
  mitos = factor(mitos)
})
summary(stage_mitos)

stage_mitos %>%
  count(mitos) %>%
  mutate(prop = round(n/sum(n)*100, 1))

# merge king and mitos stages 
stage_merged = stage_king %>%
  inner_join(stage_mitos, by = c("subjectid", "feature_delta"))
dim(stage_merged) # 28,059 records 
length(unique(stage_merged$subjectid)) # 3,059 patients 


# merge with survival data 
# death = stage 5 in MiToS and King's staging system  

surv = read_csv('PROACT_preprocessed/survival.csv')
names(surv) = tolower(names(surv))
dim(surv)
length(unique(surv$subjectid)) # 9080 patients 

deceased = surv %>%
  filter(status == 1) %>%
  filter(subjectid %in% stage_merged$subjectid) %>%
  rename(feature_delta = time_event) %>%
  mutate(king = 5, mitos = 5) %>%
  mutate(subjectid = as.character(subjectid), 
         king = factor(king),
         mitos = factor(mitos)) %>%
  select(subjectid, feature_delta, king, mitos)

stage_final = bind_rows(stage_merged, deceased)
summary(stage_final)

# In some patients (turned out to be 4), death coincided with the time of ALSFRS record (8 records) -> exclude these records.   
stage_final %>%
  group_by(subjectid, feature_delta) %>%
  count() %>%
  filter(n > 1) -> temp

stage_final %>%
  inner_join(temp, by = c("subjectid", "feature_delta")) %>%
  arrange(subjectid) -> temp2
  
stage = stage_final %>%
  anti_join(temp2, by=c("subjectid","feature_delta","king","mitos"))

length(unique(stage$subjectid)) # 3059 patients 
dim(stage) # 28,741 records 

write_csv(stage, "PROACT_preprocessed/stage_king_mitos.csv")


























