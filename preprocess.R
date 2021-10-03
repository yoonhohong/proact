# Data preprocessing   
# Source datasets (see README.md)   
# Output data files (see README.md)   

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# Load packages
library(tidyverse)
library(survival)
library(broom)
library(purrr)

# Import source datasets: training1, training2, leaderboard, validation 
data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.all <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)
length(unique(data.all$SubjectID)) # 10723 patients 
dim(data.all) # 4,456,146 records 

## type conversion; feature_delta, factor to numeric
data.all <- within(data.all, {
  SubjectID = as.character(SubjectID)
  feature_delta = round((as.numeric(feature_delta)/365)*12, 2)
  form_name = factor(form_name)
})
## exclude exact duplicates 
# data.all[duplicated(data.all),]
data.all = distinct(data.all)
dim(data.all) # 4,417,730 records 

## inconsistent values: exclude records with the same SubjectID, form_name, feature_name, 
## feature_unit and feature_delta, but different feature_value 
temp = data.all %>%
  count(SubjectID, form_name, feature_name, feature_unit, feature_delta) %>%
  filter(n == 1) %>%
  select(-n)

data.all = data.all %>%
  inner_join(temp, by = c("SubjectID", "form_name", "feature_name",
                          "feature_unit", "feature_delta"))

## exclude records with feature_delta < 0 
data.all = data.all %>%
  filter(feature_delta >= 0)
length(unique(data.all$SubjectID)) # 10,723 patients 
dim(data.all) # 4,135,461 records 

# exclude patients with neither none or incomplte demographic information 
demographic = data.all %>% filter(form_name == "Demographic") %>% 
  select(SubjectID, feature_name, feature_value)
demographic = spread(demographic, feature_name, feature_value) 
demographic = within(demographic, {
  Age = round(as.numeric(Age))
  Gender = factor(Gender)
  Race = factor(Race)
})
length(unique(demographic$SubjectID)) # 8653 patients 
demographic = demographic[complete.cases(demographic),] 
length(unique(demographic$SubjectID)) # 8646 patients 
data.all_1 = data.all %>%
  filter(SubjectID %in% demographic$SubjectID)

# exclude patients with neither none or incomplete alshx data 
# onset_delta, diag_delta, onset_site
alshx = data.all_1 %>% filter(form_name == "ALSHX") %>% 
  select(SubjectID, feature_name, feature_value) 
alshx = spread(alshx, feature_name, feature_value)
length(unique(alshx$SubjectID)) # 7227 patients 
alshx = alshx[complete.cases(alshx),]
length(unique(alshx$SubjectID)) # 4457 patients 

# exclude records with errorneous diag_delta or onset_delta values 
alshx = within(alshx, {
  diag_delta = as.numeric(as.character(diag_delta))
  onset_delta = as.numeric(as.character(onset_delta))
  onset_site = factor(onset_site)
})
alshx = alshx %>%
  filter(diag_delta <= 0) %>%
  filter(onset_delta <= 0)
length(unique(alshx$SubjectID)) # 4453 patients 
alshx = within(alshx, {
  diag_delta = -round((diag_delta/365)*12,2)
  onset_delta = -round((onset_delta/365)*12,2)
  onset2dx = onset_delta - diag_delta
})
# exclude records with onset2dx < 0  
alshx = alshx %>%
  filter(onset2dx >= 0)
# factor collapse 
alshx$onset_site = fct_collapse(alshx$onset_site, 
                                Nonbulbar = c("Limb", 
                                              "Limb and Bulbar",
                                         "Other"))

length(unique(alshx$SubjectID)) # 4442 patients 

data.all_2 = data.all_1 %>%
  filter(SubjectID %in% alshx$SubjectID)

# ALSFRS
alsfrs <- data.all_2 %>%
  filter(form_name=="ALSFRS") %>%
  select(-c(form_name,feature_unit))
## type conversion  
alsfrs$feature_value <- as.numeric(alsfrs$feature_value)
alsfrs$feature_name = factor(alsfrs$feature_name)
## convert long to wide format 
alsfrs_wide <- spread(alsfrs,feature_name,feature_value)
dim(alsfrs_wide) # 37022 records 
length(unique(alsfrs_wide$SubjectID)) # 4082 patients 
## exclude records with both Q5a and Q5b item scores recorded 
temp = alsfrs_wide
temp %>% 
  filter(!is.na(Q5a_Cutting_without_Gastrostomy)) %>%
  filter(!is.na(Q5b_Cutting_with_Gastrostomy)) %>%
  count(SubjectID, feature_delta) -> temp2
dim(temp2) # 14 records 
temp %>%
  anti_join(temp2, by = c("SubjectID", "feature_delta")) -> temp3
length(unique(temp3$SubjectID)) # 4082 patients 

## create Gastrostomy field (TRUE/FALSE)  
temp4 = temp3 %>%
  mutate(Gastrostomy = ifelse(is.na(Q5b_Cutting_with_Gastrostomy), F, T)) %>%
  select(-c(Q5a_Cutting_without_Gastrostomy, Q5b_Cutting_with_Gastrostomy))
alsfrs_wide = temp4
dim(alsfrs_wide) # 37008 records 
length(unique(alsfrs_wide$SubjectID)) # 4082 patients 

# ALSFRS orig vs. rev records 
# ALSFRS orig 
temp = alsfrs_wide %>%
  select(-c(ALSFRS_R_Total, 
            R1_Dyspnea, R2_Orthopnea, 
            R3_Respiratory_Insufficiency, 
            respiratory_R))
# numerous NA in Q10_Respiratory
# probably error in filling in the item score 
# replace the item score with the respiratory dimension score  
temp$Q10_Respiratory = ifelse(is.na(temp$Q10_Respiratory), 
                                    temp$respiratory, 
                                    temp$Q10_Respiratory) 
alsfrs_orig_wide = temp[complete.cases(temp),] 

dim(alsfrs_orig_wide) # 36971 records 
length(unique(alsfrs_orig_wide$SubjectID)) # 4082 patients 
alsfrs_orig_wide = alsfrs_orig_wide %>%
  mutate(motor = hands + leg + trunk) %>%
  rename(bulbar = mouth) %>%
  select(-c(hands, leg, trunk))
alsfrs_orig_wide = alsfrs_orig_wide[c(1:3,5,7:14,6,4,17,15,16)]
write.csv(alsfrs_orig_wide, "ALSFRS_orig.csv",
          row.names = F, quote = F)

# ALSFRS rev 
temp = alsfrs_wide %>%
  select(-c(ALSFRS_Total, 
            Q10_Respiratory, respiratory)) %>%
  mutate(motor = hands + leg + trunk) %>%
  rename(bulbar = mouth, respiratory = respiratory_R) %>%
  select(-c(hands, leg, trunk))

dim(temp) # 37008 records 
alsfrs_rev_wide = temp[complete.cases(temp),] 
dim(alsfrs_rev_wide) # 28059 records 
length(unique(alsfrs_rev_wide$SubjectID)) # 3,059 patients 
alsfrs_rev_wide = alsfrs_rev_wide[c(1:3,5:16,4,19,17,18)]
write.csv(alsfrs_rev_wide, "ALSFRS_rev.csv",
          row.names = F, quote = F)


# Calculate aggregate and meta-features of ALSFRS orig & rev 
# from records for the first 3 months
# aggregated and meta-features: mean and linear regression slope 
alsfrs_orig_3mo = alsfrs_orig_wide %>%
  filter(feature_delta <= 3)
length(unique(alsfrs_orig_3mo$SubjectID)) # 4081 patients 
dim(alsfrs_orig_3mo) # 12911 records  

# Gastrostomy == T if patients underwent gastrostomy during the first 3 mo 
alsfrs_orig_3mo_aggr = alsfrs_orig_3mo %>%
  group_by(SubjectID) %>%
  summarise(n=n(),
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta),
            Gastrostomy = ifelse(any(Gastrostomy == T),T,F),
            ALSFRS_Total = mean(ALSFRS_Total), 
            Q1_Speech = mean(Q1_Speech), 
            Q2_Salivation = mean(Q2_Salivation), 
            Q3_Swallowing = mean(Q3_Swallowing), 
            Q4_Handwriting = mean(Q4_Handwriting), 
            Q5_Cutting = mean(Q5_Cutting), 
            Q6_Dressing_and_Hygiene = mean(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed = mean(Q7_Turning_in_Bed),
            Q8_Walking = mean(Q8_Walking),
            Q9_Climbing_Stairs = mean(Q9_Climbing_Stairs),
            Q10_Respiratory = mean(Q10_Respiratory))
# estimate alsfrs slope with linear regression, 
# select subjects with interval >= 1.5 mo
temp2_orig = alsfrs_orig_3mo_aggr %>%
  filter(interval >= 1.5)
length(unique(temp2_orig$SubjectID)) # 3102 patients 
length(unique(temp2_orig$SubjectID))/length(unique(alsfrs_orig_3mo$SubjectID)) # 76% 
temp3_orig = alsfrs_orig_3mo %>%
  filter(SubjectID %in% temp2_orig$SubjectID) 
# Apply linear regression by SubjectID
# slope_total
coef_model_total_orig = temp3_orig %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(ALSFRS_Total ~ feature_delta, 
                     data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) 
alsfrs_orig_slope_total = coef_model_total_orig %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) %>%
  rename(slope_total = estimate)
# slope_bulbar
coef_model_bulbar_orig = temp3_orig %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(bulbar ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) 
alsfrs_orig_slope_bulbar = coef_model_bulbar_orig %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) %>%
  rename(slope_bulbar = estimate)
# slope_motor
coef_model_motor_orig = temp3_orig %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(motor ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) 
alsfrs_orig_slope_motor = coef_model_motor_orig %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) %>%
  rename(slope_motor = estimate)
# slope_respiratory
coef_model_respiratory_orig = temp3_orig %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(respiratory ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) 
alsfrs_orig_slope_respiratory = coef_model_respiratory_orig %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) %>%
  rename(slope_respiratory = estimate)


# Merge alsfrs_orig_3mo_aggr and 
# alsfrs_orig_slope_total and 
# alsfrs_orig_slope_bulbar and _motor and _respiratory 
alsfrs_orig_3mo_aggr_slope = alsfrs_orig_3mo_aggr %>%
  left_join(alsfrs_orig_slope_total, by = "SubjectID") %>%
  left_join(alsfrs_orig_slope_bulbar, by = "SubjectID") %>%
  left_join(alsfrs_orig_slope_motor, by = "SubjectID") %>%
  left_join(alsfrs_orig_slope_respiratory, by = "SubjectID")

alsfrs_orig_3mo_aggr_slope = alsfrs_orig_3mo_aggr_slope %>%
  select(-c(n,first, last, interval)) 


# ALSFRS rev 
# Select records for the first 3 months
# aggregated and meta-features: mean and linear regression slope 
alsfrs_rev_3mo = alsfrs_rev_wide %>%
  filter(feature_delta <= 3)
length(unique(alsfrs_rev_3mo$SubjectID)) # 3058 patients 
dim(alsfrs_rev_3mo) # 8707 records  

# Gastrostomy == T if patients underwent gastrostomy during the first 3 mo 
alsfrs_rev_3mo_aggr = alsfrs_rev_3mo %>%
  group_by(SubjectID) %>%
  summarise(n=n(),
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta),
            Gastrostomy = ifelse(any(Gastrostomy == T), T, F),
            ALSFRS_Total = mean(ALSFRS_R_Total), 
            Q1_Speech = mean(Q1_Speech), 
            Q2_Salivation = mean(Q2_Salivation), 
            Q3_Swallowing = mean(Q3_Swallowing), 
            Q4_Handwriting = mean(Q4_Handwriting), 
            Q5_Cutting = mean(Q5_Cutting), 
            Q6_Dressing_and_Hygiene = mean(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed = mean(Q7_Turning_in_Bed),
            Q8_Walking = mean(Q8_Walking),
            Q9_Climbing_Stairs = mean(Q9_Climbing_Stairs),
            R1_Dyspnea = mean(R1_Dyspnea), 
            R2_Orthopnea = mean(R2_Orthopnea),
            R3_Respiratory_Insufficiency = mean(R3_Respiratory_Insufficiency))

# estimate alsfrs slope with linear regression, 
temp_rev = alsfrs_rev_3mo_aggr
# select subjects with interval >= 1.5 mo
temp2_rev = alsfrs_rev_3mo_aggr %>%
  filter(interval >= 1.5)
length(unique(temp2_rev$SubjectID)) # 2339 patients 
length(unique(temp2_rev$SubjectID))/length(unique(alsfrs_rev_3mo$SubjectID)) # 77%
temp3_rev = alsfrs_rev_3mo %>%
  filter(SubjectID %in% temp2_rev$SubjectID)
# Apply linear regression by SubjectID
# slope_total
coef_model_total_rev = temp3_rev %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(ALSFRS_R_Total ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) 
alsfrs_rev_slope_total = coef_model_total_rev %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) %>%
  rename(slope_total = estimate)
# slope_bulbar
coef_model_bulbar_rev = temp3_rev %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(bulbar ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) 
alsfrs_rev_slope_bulbar = coef_model_bulbar_rev %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) %>%
  rename(slope_bulbar = estimate)
# slope_motor
coef_model_motor_rev = temp3_rev %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(motor ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) 
alsfrs_rev_slope_motor = coef_model_motor_rev %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) %>%
  rename(slope_motor = estimate)
# slope_respiratory
coef_model_respiratory_rev = temp3_rev %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(respiratory ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) 
alsfrs_rev_slope_respiratory = coef_model_respiratory_rev %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) %>%
  rename(slope_respiratory = estimate)


# Merge alsfrs_rev_3mo_aggr and 
# alsfrs_rev_slope_total and 
# alsfrs_rev_slope_bulbar and _motor and _respiratory 
alsfrs_rev_3mo_aggr_slope = alsfrs_rev_3mo_aggr %>%
  left_join(alsfrs_rev_slope_total, by = "SubjectID") %>%
  left_join(alsfrs_rev_slope_bulbar, by = "SubjectID") %>%
  left_join(alsfrs_rev_slope_motor, by = "SubjectID") %>%
  left_join(alsfrs_rev_slope_respiratory, by = "SubjectID")

alsfrs_rev_3mo_aggr_slope = alsfrs_rev_3mo_aggr_slope %>% 
  select(-c(n,first, last, interval)) 

# FVC & SVC  
# FVC
fvc = data.all_22 %>% # data.all2: data (distinct, consistent) with full demographic and alshx (filtered)
  filter(form_name == "FVC") %>% 
  filter(feature_name == "fvc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta)
fvc$feature_value = round(as.numeric(fvc$feature_value))
fvc = fvc[complete.cases(fvc),]
dim(fvc) # 21289 records 
length(unique(fvc$SubjectID)) # 3405 patients 
write.csv(fvc, "fvc.csv", row.names = F, quote = F)

# FVC_3mo 
# Extract data with feature_delta < 92
fvc_3mo = fvc %>% 
  filter(feature_delta <= 3)
length(unique(fvc_3mo$SubjectID)) # 3403 patients 
dim(fvc_3mo) # 10299 records 

# Calculate fvc_mean, _min, _max 
fvc_3mo_aggr = fvc_3mo %>%
  group_by(SubjectID) %>% 
  arrange(feature_delta) %>%
  summarise(fvc = mean(feature_value), 
            n=n(),
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta))

# calculate fvc_slope with linear regression, 
# select subjects with interval >= 1.5 mo
temp2 = fvc_3mo_aggr %>%
  filter(interval >= 1.5)
length(unique(temp2$SubjectID)) # 2289 patients 
length(unique(temp2$SubjectID))/length(unique(fvc_3mo$SubjectID)) # 67%
temp3 = fvc_3mo %>%
  filter(SubjectID %in% temp2$SubjectID) 
# Apply linear regression by SubjectID
coef_fvc = temp3 %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(feature_value ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef)
fvc_slope = coef_fvc %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate)
names(fvc_slope)[[2]] = "slope_fvc"

# Merge fvc_3mo_aggr and fvc_slope_tab
fvc_3mo_aggr_slope = merge(fvc_slope, 
                              fvc_3mo_aggr, 
                              by="SubjectID", all.y = T) 
fvc_3mo_aggr_slope = fvc_3mo_aggr_slope %>%
  select(-c(n,first,last,interval))


# SVC 
svc = data.all_2 %>% # data (distinct, consistent) with full demographic and alshx (filtered)
  filter(form_name == "SVC") %>% 
  filter(feature_name == "svc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta)
svc$feature_value = round(as.numeric(svc$feature_value))
length(unique(svc$SubjectID)) # 688 patients 
dim(svc) # 4769 records 
write.csv(svc, "svc.csv", row.names = F, quote = F)

# SVC_3mo 
# Extract data with feature_delta <= 3 mo
svc_3mo = svc %>% 
  filter(feature_delta <= 3)
length(unique(svc_3mo$SubjectID)) # 688 patients
dim(svc_3mo) # 2035 records 

# Calculate svc_mean  
svc_3mo_aggr = svc_3mo %>%
  group_by(SubjectID) %>% 
  arrange(feature_delta) %>%
  summarise(svc = mean(feature_value), 
            n=n(), 
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta))

# Calculate svc_slope with linear regression, 
# only in cases with interval >= 1.5 mo  
temp = svc_3mo_aggr %>%
  filter(interval >= 1.5)
length(unique(temp$SubjectID)) # 559 patients 
length(unique(temp$SubjectID))/length(unique(svc_3mo$SubjectID)) # 81% 
svc_3mo_sub = svc_3mo %>%
  filter(SubjectID %in% temp$SubjectID) 

# Apply linear regression by SubjectID
svc_3mo_sub %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(feature_value ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) -> temp
names(temp)[[2]] = "slope_svc"
svc_slope = temp
# Merge svc.aggr and slope.tab
svc_3mo_aggr_slope = merge(svc_slope, 
                           svc_3mo_aggr, 
                           by="SubjectID", all.y = T)
svc_3mo_aggr_slope = svc_3mo_aggr_slope %>%
  select(-c(n,first,last,interval))
  


# FamilyHx -> famhx
data.all_2 %>% filter(form_name == "FamilyHx") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
temp$feature_name = factor(temp$feature_name)
temp$feature_value = factor(temp$feature_value)

famhx = spread(temp, feature_name, feature_value)
dim(famhx) # 488 patients


# Riluzole
data.all_2 %>% filter(form_name == "Riluzole") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
temp$feature_name = factor(temp$feature_name)
temp$feature_value = factor(temp$feature_value)
riluzole = spread(temp, feature_name, feature_value)
dim(riluzole) # 2861 patients


# Treatment group: active vs. placebo 
data.all_2 %>% filter(form_name == "Treatment") %>%
  select(SubjectID, feature_name, feature_value) -> temp
tx = spread(temp, feature_name, feature_value)
tx$treatment_group = factor(tx$treatment_group)
dim(tx) # 3879 patients 



# Vitals 
vitals = data.all_2 %>% filter(form_name == "Vitals") %>%
  select(SubjectID, feature_name, feature_value,  
         feature_delta)
vitals$feature_name = factor(vitals$feature_name)
vitals$feature_value = as.numeric(vitals$feature_value)

# BMI 
# 초기 3개월 평균값 
bmi = vitals %>%
  filter(feature_name  == "BMI") 
bmi_wide = spread(bmi, key = feature_name, value = feature_value)
range(bmi_wide$feature_delta)
length(unique(bmi_wide$SubjectID)) # 2195 patients 

bmi_3mo = bmi_wide %>%
  filter(feature_delta <= 3)
bmi = bmi_3mo %>%
  group_by(SubjectID) %>%
  summarise(bmi = mean(BMI)*10000)

# Weight 
wt = vitals %>%
  filter(feature_name  == "weight") 
wt_wide = spread(wt, key = feature_name, value = feature_value)
dim(wt_wide) # 27292 records
length(unique(wt_wide$SubjectID)) # 4205 patients 
wt_wide = wt_wide[,c(1,3,2)]

write.csv(wt_wide, "weight.csv", quote = F, row.names = F)

# bp_diastolic, bp_systolic, pulse, respiratory_rate, temperature
vitalsign = vitals %>%
  filter(feature_name %in% 
           c("bp_systolic", "bp_diastolic", "pulse", 
             "respiratory_rate", "temperature"))

vitalsign_wide = spread(vitalsign, key = feature_name, value = feature_value)
dim(vitalsign_wide) # 31941 records 
length(unique(vitalsign_wide$SubjectID)) # 3622 patients 

vitalsign_wide %>% 
  rename(dbp = bp_diastolic, 
       sbp = bp_systolic, 
       hr = pulse, 
       rr = respiratory_rate, 
       temp = temperature) -> temp
vitalsign_wide = temp
write.csv(vitalsign_wide, "vitalsign.csv", quote=F, row.names = F)

# Calculate mean vital sign over the first 3 mo  
vitalsign_3mo_aggr = vitalsign_wide %>%
  filter(feature_delta <= 3) %>%
  group_by(SubjectID) %>% 
  summarise(dbp = mean(dbp), 
            sbp = mean(sbp), 
            hr = mean(hr), 
            rr = mean(rr),
            temp = mean(temp))
dim(vitalsign_3mo_aggr) # 3603 patients


# Lab 
lab = data.all_2 %>% 
  filter(form_name == "Lab Test") %>%
  select(-c(form_name, feature_unit))
lab$feature_name = factor(lab$feature_name)
lab$feature_value = as.numeric(lab$feature_value)

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
lab_wide = spread(lab_sub, feature_name, feature_value)
length(unique(lab_wide$SubjectID)) # 3792 patients 
dim(lab_wide) # 32557 records 

write.csv(lab_wide, "lab.csv", quote=F, row.names = F)

# Calculate mean 
lab_aggr = lab_sub %>%
  filter(feature_delta <= 3) %>%
  group_by(SubjectID, feature_name) %>%
  nest() %>%
  mutate(mean = map_dbl(data, ~mean(.x$feature_value))) %>%
  select(-data)
lab_3mo_aggr = spread(lab_aggr, key = feature_name, value = mean)

dim(lab_3mo_aggr) # 3781 patients

# merge datasets 
merged_orig = demographic %>%
  inner_join(alshx, by = "SubjectID") %>% 
  inner_join(alsfrs_orig_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(fvc_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(svc_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(famhx, by = "SubjectID") %>%
  left_join(riluzole, by = "SubjectID") %>%
#  left_join(tx, by = "SubjectID") %>%
  left_join(bmi, by = "SubjectID") %>%
  left_join(vitalsign_3mo_aggr, by = "SubjectID") %>%
  left_join(lab_3mo_aggr, by = "SubjectID") 

merged_rev = demographic %>%
  inner_join(alshx, by = "SubjectID") %>% 
  inner_join(alsfrs_rev_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(fvc_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(svc_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(famhx, by = "SubjectID") %>%
  left_join(riluzole, by = "SubjectID") %>%
#  left_join(tx, by = "SubjectID") %>%
  left_join(bmi, by = "SubjectID") %>%
  left_join(vitalsign_3mo_aggr, by = "SubjectID") %>%
  left_join(lab_3mo_aggr, by = "SubjectID")

write.csv(merged_orig, "PROACT_preprocessed_orig.csv", quote = F, row.names = F)
write.csv(merged_rev, "PROACT_preprocessed_rev.csv", quote = F, row.names = F)

# The End # 
