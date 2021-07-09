# Data preprocessing: cleaning, dummy variables, meta feaetures 
# Except for imputation 

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# Load packages
library(dplyr)
library(tidyr)
library(survival)

# Read all datasets: training, leaderboard, validation 
data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.allforms <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)

# Type conversion; feature_delta, factor to numeric
data.all <- data.allforms
data.all$SubjectID = as.character(data.all$SubjectID)
data.all$feature_delta = as.numeric(data.all$feature_delta)
data.all$form_name = factor(data.all$form_name)
length(unique(data.all$SubjectID)) # 10723 patients
dim(data.all) # 4,456,146 records 
levels(data.all$form_name)
range(data.all$feature_delta, na.rm = T) # -468 days to 3,742 days


# Demographic
data.all %>% filter(form_name == "Demographic") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
demographic = spread(temp, feature_name, feature_value) 
length(unique(demographic$SubjectID)) # 8653 patients 
demographic -> temp
temp$Age = round(as.numeric(temp$Age))
temp$Gender = factor(temp$Gender)
temp$Race = factor(temp$Race)
demographic <- temp
str(demographic)
demographic %>%
  filter(Gender == "") # 6 patients 
demographic <- demographic %>% # excluded patients with no gender information
  filter(!(Gender == "")) 
demographic$Gender = factor(demographic$Gender)
levels(demographic$Race)
range(demographic$Age)
demographic %>%
  filter(is.na(Age))
demographic %>%
  filter(!is.na(Age)) -> demographic # excluded patients with no age information 
summary(demographic)
dim(demographic)
demographic = demographic[complete.cases(demographic),] # 8646 patients 
write.csv(demographic, 'demographic.csv', row.names = F, quote = F)

# ALSFRS
alsfrs <- data.all %>%
  filter(form_name=="ALSFRS", )
alsfrs = alsfrs %>%
  select(-c(form_name,feature_unit))
alsfrs$feature_value <- as.numeric(alsfrs$feature_value)
# exclude alsfrs records with feature_delta < 0 
range(alsfrs$feature_delta)
alsfrs = alsfrs %>%
  filter(feature_delta >= 0) 

# exclude duplicates
dim(alsfrs)
temp = distinct(alsfrs)
dim(temp)
# exclude ALSFRS records with the same feature_delta 
# but having different feature_value (partial duplicates)
# finding partial duplicates
dup = temp %>%
  count(SubjectID, feature_name, feature_delta) %>%
  filter(n > 1) 
# create unique ids for partial duplicates
dup_ids = dup %>%
  mutate(ids = paste(SubjectID, 
                     feature_name, 
                     feature_delta, sep = "_"))
# create unique ids for original data
temp2 = temp %>%
  mutate(ids = paste(SubjectID, 
                     feature_name, 
                     feature_delta, sep = "_"))
# filter    
temp3 = temp2 %>%
  filter(!(ids %in% dup_ids$ids)) %>%
  select(-ids)
# validate 
temp3 %>%
  count(SubjectID, feature_name, feature_delta) %>%
  filter(n > 1) 

# temp$id_delta = paste(temp$SubjectID, 
#                       temp$feature_delta, sep = "_")
# temp2 = temp %>%
#   select(-feature_value)
# temp3 = temp2[duplicated(temp2),]
# duplicated_id_delta = unique(temp3$id_delta)
# temp4 = temp %>%
#   filter(!(id_delta %in% duplicated_id_delta)) %>%
#   select(-id_delta)
temp3$feature_name = factor(temp3$feature_name)
alsfrs_wide <- spread(temp3,feature_name,feature_value)

# Exclude records with both Q5a and Q5b item scores
temp = alsfrs_wide
w_gastro = temp %>%
  filter(!is.na(Q5b_Cutting_with_Gastrostomy)) 
wo_gastro = temp %>%
  filter(!is.na(Q5a_Cutting_without_Gastrostomy)) 
temp2 = intersect(w_gastro, wo_gastro)
dim(temp2) # 797 records 

temp3 = setdiff(temp, temp2)
w_gastro = temp3 %>%
  filter(!is.na(Q5b_Cutting_with_Gastrostomy)) 
wo_gastro = temp3 %>%
  filter(!is.na(Q5a_Cutting_without_Gastrostomy)) 
# validate
intersect(w_gastro, wo_gastro)

temp3 %>%
  filter(is.na(Q5a_Cutting_without_Gastrostomy)) %>%
  filter(!(Q5_Cutting == Q5b_Cutting_with_Gastrostomy))

temp3 %>%
  filter(is.na(Q5b_Cutting_with_Gastrostomy)) %>%
  filter(!(Q5_Cutting == Q5a_Cutting_without_Gastrostomy))

temp4 = temp3 %>%
  mutate(Gastrostomy = ifelse(is.na(Q5b_Cutting_with_Gastrostomy), F, T)) %>%
  select(-c(Q5a_Cutting_without_Gastrostomy, Q5b_Cutting_with_Gastrostomy))

alsfrs_wide = temp4
dim(alsfrs_wide) # 58487 records 

# ALSFRS original vs. revised records 
library(visdat)
vis_miss(alsfrs_wide, warn_large_data = F)

# orig = alsfrs_wide %>%
#   filter(!is.na(ALSFRS_Total)) 
# dim(orig) # 58470 records 
# rev = alsfrs_wide %>%
#   filter(!is.na(ALSFRS_R_Total))
# dim(rev) # 29395 records 
# both = intersect(orig, rev)
# dim(both) # 29393 records 

# ALSFRS original 
temp = alsfrs_wide %>%
  select(-c(ALSFRS_R_Total, 
            R1_Dyspnea, R2_Orthopnea, 
            R3_Respiratory_Insufficiency, 
            respiratory_R))
vis_miss(temp, warn_large_data = F)

# NA in Q10_Respiratory
# probably error in filling in the item score 
# replaced the item score with the respiratory dimension score  
temp$Q10_Respiratory = ifelse(is.na(temp$Q10_Respiratory), 
                                    temp$respiratory, 
                                    temp$Q10_Respiratory) 

alsfrs_original_wide = temp[complete.cases(temp),] 
summary(alsfrs_original_wide)

dim(alsfrs_original_wide) # 58412 records 
length(unique(alsfrs_original_wide$SubjectID)) # 6,377 patients 
table(table(alsfrs_original_wide$SubjectID))
range(alsfrs_original_wide$feature_delta)

# ALSFRS revised 
temp = alsfrs_wide %>%
  select(-c(ALSFRS_Total, 
            Q10_Respiratory, respiratory))
alsfrs_revised_wide = temp[complete.cases(temp),] 
dim(alsfrs_revised_wide) # 29,381 records 
length(unique(alsfrs_revised_wide$SubjectID)) # 3,275 patients 
table(table(alsfrs_revised_wide$SubjectID))
range(alsfrs_revised_wide$feature_delta)

write.csv(alsfrs_original_wide, "ALSFRS_original.csv",
          row.names = F, quote = F)
write.csv(alsfrs_revised_wide, "ALSFRS_revised.csv",
          row.names = F, quote = F)

# ALSFRS original vs. revised 

# ALSFRS original 
# Select records for the first 3 months
# Meta-features: mean, min, max and linear regression slope 
alsfrs_3mo = alsfrs_original_wide %>%
  filter(((feature_delta)/365)*12 <= 3)
range(alsfrs_3mo$feature_delta)
length(unique(alsfrs_3mo$SubjectID)) # 6374 patients 
table(table(alsfrs_3mo$SubjectID)) 
dim(alsfrs_3mo) # 20,610 records  

# create meta feature; gastrostomy
# some patients underwent gastrostomy during the first 3 mo
alsfrs_3mo %>%
  group_by(SubjectID) %>%
  summarize(n = sum(Gastrostomy)) %>%
  filter(n >= 1) -> temp
dim(temp) # 958 patients 
# Gastrostomy == T if patients underwent gastrostomy during the first 3 mo 
gastro = alsfrs_3mo %>% 
  group_by(SubjectID) %>%
  summarise(Gastrostomy = ifelse(any(Gastrostomy == T), 
                                 T, F)) 

alsfrs_3mo_aggr = alsfrs_3mo %>%
  group_by(SubjectID) %>%
  summarise(n=n(),
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta),
            ALSFRS_Total_mean = mean(ALSFRS_Total), 
            Q1_Speech_mean = mean(Q1_Speech), 
            Q2_Salivation_mean = mean(Q2_Salivation), 
            Q3_Swallowing_mean = mean(Q3_Swallowing), 
            Q4_Handwriting_mean = mean(Q4_Handwriting), 
            Q5_Cutting_mean = mean(Q5_Cutting), 
            Q6_Dressing_and_Hygiene_mean = mean(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed_mean = mean(Q7_Turning_in_Bed),
            Q8_Walking_mean = mean(Q8_Walking),
            Q9_Climbing_Stairs_mean = mean(Q9_Climbing_Stairs),
            Q10_Respiratory_mean = mean(Q10_Respiratory), 
            mouth_mean = mean(mouth),
            hands_mean = mean(hands),
            leg_mean = mean(leg),
            trunk_mean = mean(trunk),
            respiratory_mean = mean(respiratory),
            ALSFRS_Total_max = max(ALSFRS_Total), 
            Q1_Speech_max = max(Q1_Speech), 
            Q2_Salivation_max = max(Q2_Salivation), 
            Q3_Swallowing_max = max(Q3_Swallowing), 
            Q4_Handwriting_max = max(Q4_Handwriting), 
            Q5_Cutting_max = max(Q5_Cutting), 
            Q6_Dressing_and_Hygiene_max = max(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed_max = max(Q7_Turning_in_Bed),
            Q8_Walking_max = max(Q8_Walking),
            Q9_Climbing_Stairs_max = max(Q9_Climbing_Stairs),
            Q10_Respiratory_max = max(Q10_Respiratory), 
            mouth_max = max(mouth),
            hands_max = max(hands),
            leg_max = max(leg),
            trunk_max = max(trunk),
            respiratory_max = max(respiratory),
            ALSFRS_Total_min = min(ALSFRS_Total), 
            Q1_Speech_min = min(Q1_Speech), 
            Q2_Salivation_min = min(Q2_Salivation), 
            Q3_Swallowing_min = min(Q3_Swallowing), 
            Q4_Handwriting_min = min(Q4_Handwriting), 
            Q5_Cutting_min = min(Q5_Cutting), 
            Q6_Dressing_and_Hygiene_min = min(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed_min = min(Q7_Turning_in_Bed),
            Q8_Walking_min = min(Q8_Walking),
            Q9_Climbing_Stairs_min = min(Q9_Climbing_Stairs),
            Q10_Respiratory_min = min(Q10_Respiratory), 
            mouth_min = min(mouth),
            hands_min = min(hands),
            leg_min = min(leg),
            trunk_min = min(trunk),
            respiratory_min = min(respiratory))

dim(alsfrs_3mo_aggr)
alsfrs_3mo_meta = merge(alsfrs_3mo_aggr, gastro, by = "SubjectID")
summary(alsfrs_3mo_meta)

# estimate alsfrs slope with linear regression, 
temp = alsfrs_3mo_meta
range(temp$interval)

library(ggplot2)
temp %>%
  ggplot(aes(interval)) +
  geom_histogram()
# select subjects with interval >= 56 days (8 wks)
temp2 = alsfrs_3mo_meta %>%
  filter(interval >= 56)
length(unique(temp2$SubjectID)) # 4672 patients 
temp3 = alsfrs_3mo %>%
  filter(SubjectID %in% temp2$SubjectID) %>%
  mutate(feature_delta_mo = feature_delta/28)
# Apply linear regression by SubjectID
lm = with(temp3, by(temp3, SubjectID, 
                       function(x) lm(ALSFRS_Total ~ feature_delta_mo, 
                                      data=x)))
alsfrs_slope = sapply(lm, coef)[2,]
alsfrs_slope_tab = data.frame(alsfrs_slope)
alsfrs_slope_tab$SubjectID = rownames(alsfrs_slope_tab)
alsfrs_slope_tab %>%
  ggplot(aes(alsfrs_slope)) + 
  geom_histogram()
quantile(alsfrs_slope_tab$alsfrs_slope)

# Merge alsfrs_3mo_meta and alsfrs_slope_tab
alsfrs_3mo_meta_slope = merge(alsfrs_slope_tab, 
                               alsfrs_3mo_meta, 
                               by="SubjectID", all.y = T) 
summary(alsfrs_3mo_meta_slope)

write.csv(alsfrs_3mo_meta_slope, "alsfrs_orig_3mo_meta_slope.csv", 
          quote=F, row.names = F)

# ALSFRS revised 
# Select records for the first 3 months
# Meta-features: mean, min, max and linear regression slope 
alsfrs_3mo = alsfrs_revised_wide %>%
  filter(((feature_delta)/365)*12 <= 3)
range(alsfrs_3mo$feature_delta)
length(unique(alsfrs_3mo$SubjectID)) # 3273 patients 
table(table(alsfrs_3mo$SubjectID)) 
dim(alsfrs_3mo) # 9253 records  

# create meta feature; gastrostomy
# some patients underwent gastrostomy during the first 3 mo
alsfrs_3mo %>%
  group_by(SubjectID) %>%
  summarize(n = sum(Gastrostomy)) %>%
  filter(n >= 1) -> temp
dim(temp) # 823 patients 
# Gastrostomy == T if patients underwent gastrostomy during the first 3 mo 
gastro = alsfrs_3mo %>% 
  group_by(SubjectID) %>%
  summarise(Gastrostomy = ifelse(any(Gastrostomy == T), T, F)) 

alsfrs_3mo_aggr = alsfrs_3mo %>%
  group_by(SubjectID) %>%
  summarise(n=n(),
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta),
            ALSFRS_R_Total_mean = mean(ALSFRS_R_Total), 
            Q1_Speech_mean = mean(Q1_Speech), 
            Q2_Salivation_mean = mean(Q2_Salivation), 
            Q3_Swallowing_mean = mean(Q3_Swallowing), 
            Q4_Handwriting_mean = mean(Q4_Handwriting), 
            Q5_Cutting_mean = mean(Q5_Cutting), 
            Q6_Dressing_and_Hygiene_mean = mean(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed_mean = mean(Q7_Turning_in_Bed),
            Q8_Walking_mean = mean(Q8_Walking),
            Q9_Climbing_Stairs_mean = mean(Q9_Climbing_Stairs),
            R1_Dyspnea_mean = mean(R1_Dyspnea), 
            R2_Orthopnea_mean = mean(R2_Orthopnea),
            R3_Respiratory_Insufficiency_mean = mean(R3_Respiratory_Insufficiency),
            mouth_mean = mean(mouth),
            hands_mean = mean(hands),
            leg_mean = mean(leg),
            trunk_mean = mean(trunk),
            respiratory_R_mean = mean(respiratory_R),
            ALSFRS_R_Total_max = max(ALSFRS_R_Total), 
            Q1_Speech_max = max(Q1_Speech), 
            Q2_Salivation_max = max(Q2_Salivation), 
            Q3_Swallowing_max = max(Q3_Swallowing), 
            Q4_Handwriting_max = max(Q4_Handwriting), 
            Q5_Cutting_max = max(Q5_Cutting), 
            Q6_Dressing_and_Hygiene_max = max(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed_max = max(Q7_Turning_in_Bed),
            Q8_Walking_max = max(Q8_Walking),
            Q9_Climbing_Stairs_max = max(Q9_Climbing_Stairs),
            R1_Dyspnea_max = max(R1_Dyspnea), 
            R2_Orthopnea_max = max(R2_Orthopnea),
            R3_Respiratory_Insufficiency_max = max(R3_Respiratory_Insufficiency),
            mouth_max = max(mouth),
            hands_max = max(hands),
            leg_max = max(leg),
            trunk_max = max(trunk),
            respiratory_R_max = max(respiratory_R),
            ALSFRS_R_Total_min = min(ALSFRS_R_Total), 
            Q1_Speech_min = min(Q1_Speech), 
            Q2_Salivation_min = min(Q2_Salivation), 
            Q3_Swallowing_min = min(Q3_Swallowing), 
            Q4_Handwriting_min = min(Q4_Handwriting), 
            Q5_Cutting_min = min(Q5_Cutting), 
            Q6_Dressing_and_Hygiene_min = min(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed_min = min(Q7_Turning_in_Bed),
            Q8_Walking_min = min(Q8_Walking),
            Q9_Climbing_Stairs_min = min(Q9_Climbing_Stairs),
            R1_Dyspnea_min = min(R1_Dyspnea), 
            R2_Orthopnea_min = min(R2_Orthopnea),
            R3_Respiratory_Insufficiency_min = min(R3_Respiratory_Insufficiency),
            mouth_min = min(mouth),
            hands_min = min(hands),
            leg_min = min(leg),
            trunk_min = min(trunk),
            respiratory_R_min = min(respiratory_R))

dim(alsfrs_3mo_aggr)
alsfrs_3mo_meta = merge(alsfrs_3mo_aggr, gastro, by = "SubjectID")
summary(alsfrs_3mo_meta)

# estimate alsfrs slope with linear regression, 
temp = alsfrs_3mo_meta
range(temp$interval)

temp %>%
  ggplot(aes(interval)) +
  geom_histogram()
# select subjects with interval >= 56 days (8 wks)
temp2 = alsfrs_3mo_meta %>%
  filter(interval >= 56)
length(unique(temp2$SubjectID)) # 2004 patients 
temp3 = alsfrs_3mo %>%
  filter(SubjectID %in% temp2$SubjectID) %>%
  mutate(feature_delta_mo = feature_delta/28)
# Apply linear regression by SubjectID
lm = with(temp3, by(temp3, SubjectID, 
                    function(x) lm(ALSFRS_R_Total ~ feature_delta_mo, 
                                   data=x)))
alsfrs_slope = sapply(lm, coef)[2,]
alsfrs_slope_tab = data.frame(alsfrs_slope)
alsfrs_slope_tab$SubjectID = rownames(alsfrs_slope_tab)
alsfrs_slope_tab %>%
  ggplot(aes(alsfrs_slope)) + 
  geom_histogram()
quantile(alsfrs_slope_tab$alsfrs_slope)

# Merge alsfrs_3mo_meta and alsfrs_slope_tab
alsfrs_3mo_meta_slope = merge(alsfrs_slope_tab, 
                              alsfrs_3mo_meta, 
                              by="SubjectID", all.y = T) 
alsfrs_r_3mo_meta_slope = alsfrs_3mo_meta_slope
summary(alsfrs_r_3mo_meta_slope)

write.csv(alsfrs_r_3mo_meta_slope, "alsfrs_revised_3mo_meta_slope.csv", 
          quote=F, row.names = F)

# FVC & SVC  
# FVC
fvc = data.all %>% 
  filter(form_name == "FVC") %>% 
  filter(feature_name == "fvc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta)
# exclude feature_delta < 0 
range(fvc$feature_delta)
fvc$feature_value = round(as.numeric(fvc$feature_value))
fvc = fvc %>%
  filter(feature_delta >= 0)
# exclude duplicates (full)
fvc = distinct(fvc)
# exclude partial duplicates  
dup_ids = fvc %>%
  count(SubjectID, feature_name, feature_delta) %>%
  filter(n>1) %>%
  mutate(ids = paste(SubjectID, feature_name, feature_delta, sep = "_"))

fvc_ids = fvc %>%
  mutate(ids = paste(SubjectID, feature_name, feature_delta, sep = "_"))

temp = fvc_ids %>%
  filter(!(ids %in% dup_ids$ids)) %>%
  select(-ids)
# validate
temp %>%
  count(SubjectID, feature_name, feature_delta) %>%
  filter(n > 1) 
fvc = temp
# exclude missing values 
summary(fvc)
fvc = fvc[complete.cases(fvc),]
dim(fvc) # 44516 records 
length(unique(fvc$SubjectID)) # 7313 patients 
range(fvc$feature_delta)

write.csv(fvc, "fvc.csv", row.names = F, quote = F)

# FVC_3mo 
# Extract data with feature_delta < 92
fvc_3mo = fvc %>% 
  filter((feature_delta/365)*12 <= 3)
length(unique(fvc_3mo$SubjectID)) # 7218 patients 
dim(fvc_3mo) # 19660 records 

# Calculate fvc_mean, _min, _max 
fvc_3mo_meta = fvc_3mo %>%
  group_by(SubjectID) %>% 
  arrange(feature_delta) %>%
  summarise(fvc_mean = mean(feature_value), 
            fvc_min = min(feature_value), 
            fvc_max = max(feature_value), 
            n=n(),
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta))
dim(fvc_3mo_meta)

# calculate fvc_slope with linear regression, 
# only cases with interval >= 8 wks   

temp = fvc_3mo_meta
range(temp$interval)

temp %>%
  ggplot(aes(interval)) +
  geom_histogram()
# select subjects with interval >= 56 days (8 wks)

temp2 = fvc_3mo_meta %>%
  filter(interval >= 56)
length(unique(temp2$SubjectID)) # 4105 patients 
temp3 = fvc_3mo %>%
  filter(SubjectID %in% temp2$SubjectID) %>%
  mutate(feature_delta_mo = feature_delta/28)
# Apply linear regression by SubjectID
lm = with(temp3, by(temp3, SubjectID, 
                    function(x) lm(feature_value ~ feature_delta_mo, 
                                   data=x)))
fvc_slope = sapply(lm, coef)[2,]
fvc_slope_tab = data.frame(fvc_slope)
fvc_slope_tab$SubjectID = rownames(fvc_slope_tab)
fvc_slope_tab %>%
  ggplot(aes(fvc_slope)) + 
  geom_histogram()
quantile(fvc_slope_tab$fvc_slope)

# Merge fvc_3mo_meta and fvc_slope_tab
fvc_3mo_meta_slope = merge(fvc_slope_tab, 
                              fvc_3mo_meta, 
                              by="SubjectID", all.y = T) 
fvc_3mo_meta_slope = fvc_3mo_meta_slope
summary(fvc_3mo_meta_slope)

write.csv(fvc_3mo_meta_slope, "fvc_3mo_meta_slope.csv", 
          quote=F, row.names = F)

# SVC 
svc = data.all %>% 
  filter(form_name == "SVC") %>% 
  filter(feature_name == "svc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta)
svc$feature_value = round(as.numeric(svc$feature_value))
# exclude full dupldates 
svc = distinct(svc)
dim(svc)
range(svc$feature_delta)
# exclude partial duplicates 
svc %>%
  count(SubjectID, feature_name, feature_delta) %>%
  filter(n > 1) 

length(unique(svc$SubjectID)) # 695 patients 
dim(svc) # 4826 records 
table(table(svc$SubjectID))

write.csv(svc, "svc.csv", row.names = F, quote = F)

# SVC_3mo 
# Extract data with feature_delta <= 3 mo
svc_3mo = svc %>% 
  filter((feature_delta/365)*12 <= 3)
length(unique(svc_3mo$SubjectID)) # 695 patients
dim(svc_3mo) # 2056 records 

# Calculate svc_mean, _min, _max 
svc_3mo_meta = svc_3mo %>%
  group_by(SubjectID) %>% 
  arrange(feature_delta) %>%
  summarise(svc_mean = mean(feature_value), 
            svc_min = min(feature_value), 
            svc_max = max(feature_value), 
            n=n(), 
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta))
dim(svc_3mo_meta) 

# Calculate svc_slope with linear regression, 
# only in cases with interval >= 8 wks 
range(svc_3mo_meta$interval)

svc_3mo_meta %>%
  ggplot(aes(interval)) + 
  geom_histogram()
temp = svc_3mo_meta %>%
  filter(interval >= 56)
svc_3mo_sub = svc_3mo %>%
  filter(SubjectID %in% temp$SubjectID) %>%
  mutate(feature_delta_mo = feature_delta/28)
  
# Apply linear regression by SubjectID
lm = with(svc_3mo_sub, by(svc_3mo_sub, SubjectID, 
                          function(x) lm(feature_value ~ 
                                           feature_delta_mo, data=x)))
svc_slope = sapply(lm, coef)[2,]
slope.tab = as.data.frame(svc_slope)
slope.tab$SubjectID = rownames(slope.tab)
# Merge svc.meta and slope.tab
svc.tab = merge(slope.tab, svc_3mo_meta, by="SubjectID", all.y = T)
write.csv(svc.tab, "svc_3mo_meta_slope.csv", quote=F, row.names = F)


# ALSHX 
# Filter form_name == "ALSHX"
data.all %>% filter(form_name == "ALSHX") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
alshx = spread(temp, feature_name, feature_value)
alshx = within(alshx, {
  diag_delta = as.numeric(as.character(diag_delta))
  onset_delta = as.numeric(as.character(onset_delta))
  onset_site = factor(onset_site)
})
temp = alshx %>%
  filter(diag_delta <= 0) %>%
  filter(onset_delta <= 0)
alshx = temp
summary(alshx)
# exclude case with missing value in onset_site
alshx = alshx[complete.cases(alshx),] 

dim(alshx) # 4453 patients 
write.csv(alshx, "als_hx.csv", quote=F, row.names=F)

# FamilyHx -> famhx
data.all %>% filter(form_name == "FamilyHx") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
temp$feature_name = factor(temp$feature_name)
temp$feature_value = factor(temp$feature_value)

famhx = spread(temp, feature_name, feature_value)
dim(famhx) # 1007 patients
famhx[duplicated(famhx),] # no duplicates  
write.csv(famhx, "family_hx.csv", quote=F, row.names=F)


# Riluzole
data.all %>% filter(form_name == "Riluzole") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
temp$feature_name = factor(temp$feature_name)
temp$feature_value = factor(temp$feature_value)
riluzole = spread(temp, feature_name, feature_value)
dim(riluzole) # 8817 patients
riluzole[duplicated(riluzole),]
write.csv(riluzole, "riluzole.csv", quote=F, row.names=F)


# Treatment group: active vs. placebo 
data.all %>% filter(form_name == "Treatment") %>%
  select(SubjectID, feature_name, feature_value) -> temp
tx = spread(temp, feature_name, feature_value)
tx$treatment_group = factor(tx$treatment_group)
dim(tx) # 9640 patients 
tx[duplicated(tx),]
write.csv(tx, "treatment_group.csv", quote = F, row.names = F)


# Vitals 

vitals = data.all %>% filter(form_name == "Vitals") %>%
  select(SubjectID, feature_name, feature_value,  
         feature_delta)
vitals$feature_name = factor(vitals$feature_name)
levels(vitals$feature_name)
vitals$feature_value = as.numeric(vitals$feature_value)
vitals = vitals %>%
  filter(feature_delta >= 0)
# exclude duplicates 
temp = unique(vitals) 

dup = temp %>%
  count(SubjectID, feature_name, feature_delta) %>%
  filter(n > 1)

dup_ids = dup %>%
  mutate(ids = paste(SubjectID,
                     feature_name, 
                     feature_delta, 
                     sep = "_"
                     ))
temp2 = temp %>%
  mutate(ids = paste(SubjectID,
                     feature_name, 
                     feature_delta, 
                     sep = "_"))
temp3 = temp2 %>%
  filter(!(temp2$ids %in% dup_ids$ids)) %>%
  select(-ids)
# validate 
temp3 %>%
  count(SubjectID, feature_name, feature_delta) %>%
  filter(n > 1)

vitals = temp3   
# BMI 
bmi = vitals %>%
  filter(feature_name  == "BMI") 
bmi_wide = spread(bmi, key = feature_name, value = feature_value)
dim(bmi_wide) # 6337
length(unique(bmi_wide$SubjectID)) # 4550
table(table(bmi_wide$SubjectID))

bmi_wide = bmi_wide[,c(1,3,2)]
range(bmi_wide$feature_delta)

write.csv(bmi_wide, "bmi.csv", quote = F, row.names = F)

# Calculate bmi_mean 
# bmi_3mo = bmi_wide %>%
#   filter((feature_delta/365)*12 <= 3) %>%
#   group_by(SubjectID) %>% 
#   summarise(BMI_3mo_mean = mean(BMI))
# dim(bmi_3mo) 


# Weight 
wt = vitals %>%
  filter(feature_name  == "weight") 
wt_wide = spread(wt, key = feature_name, value = feature_value)
dim(wt_wide) # 50,266 records
length(unique(wt_wide$SubjectID)) # 7587 patients 
table(table(wt_wide$SubjectID))
wt_wide = wt_wide[,c(1,3,2)]
range(wt_wide$feature_delta)

write.csv(wt_wide, "weight.csv", quote = F, row.names = F)

# Calculate wt_mean 
# wt_3mo = wt_wide %>%
#   filter((feature_delta/365)*12 <= 3) %>%
#   group_by(SubjectID) %>% 
#   summarise(Wt_3mo_mean = mean(weight))
# dim(bmi_3mo) # 4549 patients 


# # Height
# ht = vitals %>%
#   filter(feature_name  == "height") 
# ht_wide = spread(ht, key = feature_name, value = feature_value)
# dim(ht_wide) # 7805 records
# length(unique(ht_wide$SubjectID)) # 5485 patients 
# table(table(ht_wide$SubjectID))
# ht_wide = ht_wide[,c(1,3,2)]
# range(ht_wide$feature_delta)

# bp_diastolic, bp_systolic, pulse, respiratory_rate, temperature
vitalsign = vitals %>%
  filter(feature_name %in% 
           c("bp_systolic", "bp_diastolic", "pulse", 
             "respiratory_rate", "temperature"))

vitalsign_wide = spread(vitalsign, key = feature_name, value = feature_value)
range(vitalsign_wide$feature_delta)
dim(vitalsign_wide) # 61237 records 
length(unique(vitalsign_wide$SubjectID)) # 7043 patients 

write.csv(vitalsign_wide, "vitalsign.csv", quote=F, row.names = F)

# Calculate mean 
vitalsign_3mo_meta = vitalsign_wide %>%
  filter((feature_delta/365)*12 <= 3) %>%
  group_by(SubjectID) %>% 
  summarise(bp_diastolic_3mo_mean = mean(bp_diastolic), 
            bp_systolic_3mo_mean = mean(bp_systolic), 
            pulse_3mo_mean = mean(pulse), 
            respiratory_rate_3mo_mean = mean(respiratory_rate),
            temperature_3mo_mean = mean(temperature))
dim(vitalsign_3mo_meta) # 7024 patients
write.csv(vitalsign_3mo_meta, "vitalsign_3mo_meta.csv", quote=F, row.names = F)

# Lab 
lab = data.all %>% filter(form_name == "Lab Test") %>%
  select(-c(form_name, feature_unit))
lab$feature_name = factor(lab$feature_name)
levels(lab$feature_name)
lab$feature_value = as.numeric(lab$feature_value)
lab = lab %>%
  filter(feature_delta >= 0)
lab = distinct(lab)
# select some features
lab_sub = lab %>% 
  filter(feature_name %in% 
           c("Absolute Neutrophil Count",
             "Absolute Lymphocyte Count",
             "Absolute Monocyte Count",
             "C-Reactive Protein",
             "CK",
             "Creatinine",
             "GFR",
             "Phosphate", 
             "Phosphorus",
             "Total Cholesterol",
             "Triglycerides",
             "HDL",
             "LDL",
             "Uric Acid",
             "Urine Creatinine",
             "Urine Creatinine Clearance"
           ))
# exclude partial duplicates 
dup = lab_sub %>%
  count(SubjectID, feature_name, feature_delta) %>%
  filter(n > 1)
dup_ids = dup %>%
  mutate(ids = paste(SubjectID, feature_name, feature_delta, 
                     sep = "_"))
lab_ids = lab_sub %>%
  mutate(ids = paste(SubjectID, feature_name, feature_delta, 
                     sep = "_"))
lab_sub = lab_ids %>%
  filter(!(ids %in% dup_ids$ids)) %>%
  select(-ids)
# validate
lab_sub %>%
  count(SubjectID, feature_name, feature_delta) %>%
  filter(n > 1)
lab.tab = spread(lab_sub, feature_name, feature_value)
range(lab.tab$feature_delta)
length(unique(lab.tab$SubjectID)) # 7776 patients 
dim(lab.tab) # 66638 records 

write.csv(lab.tab, "lab.csv", quote=F, row.names = F)

# Calculate mean 
lab_3mo_mean = lab.tab %>%
  filter((feature_delta/365)*12 <= 3) %>%
  group_by(SubjectID) %>% 
  summarise("Absolute Neutrophil Count" = mean(`Absolute Neutrophil Count`),
            "Absolute Lymphocyte Count" = mean(`Absolute Lymphocyte Count`),
            "Absolute Monocyte Count" = mean(`Absolute Monocyte Count`),
            "C-Reactive Protein" = mean(`C-Reactive Protein`),
            CK = mean(CK),
            Creatinine = mean(Creatinine),
            GFR = mean(GFR),
            Phosphate = mean(Phosphate),
            Phosphorus = mean(Phosphorus),
            "Total Cholesterol" = mean(`Total Cholesterol`),
            Triglycerides = mean(Triglycerides),
            HDL = mean(HDL),
            LDL = mean(LDL),
            "Uric Acid" = mean(`Uric Acid`),
            "Urine Creatinine" = mean(`Urine Creatinine`),
            "Urine Creatinine Clearance" = mean(`Urine Creatinine Clearance`))

dim(lab_3mo_mean) # 7712 patients
summary(lab_3mo_mean)

write.csv(lab_3mo_mean, "lab_3mo_mean.csv", quote = F, row.names = F)

# The End # 
