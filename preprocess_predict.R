# Data preprocessing   

# Set working directory 
setwd("~/Dropbox/Github/proact")

# Load packages
library(tidyverse)
library(broom)

# Import source datasets: training1, training2, leaderboard, validation 
data.allforms_training<-read.delim("~/Dropbox/ALSmaster/PROACT_data/PROACT_rawdata/all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("~/Dropbox/ALSmaster/PROACT_data/PROACT_rawdata/all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("~/Dropbox/ALSmaster/PROACT_data/PROACT_rawdata/all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("~/Dropbox/ALSmaster/PROACT_data/PROACT_rawdata/all_forms_PROACT_validation_full.txt",sep="|", header=T)

# Combine datasets  
data.all <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)
length(unique(data.all$SubjectID)) # 10723 patients 
dim(data.all) # 4,456,146 records 

# Preprocess 1 (for prediction & staging projects both)

## Data type and time unit conversion; feature_delta (days -> months)
data.all <- within(data.all, {
  SubjectID = as.character(SubjectID)
  feature_delta = round((as.numeric(feature_delta)/365)*12, 2)
  form_name = factor(form_name)
})
## Exclude exact duplicates 
data.all = distinct(data.all)
dim(data.all) # 4,417,730 records (excluding 38,416 records)

## Exclude inconsistent values: records with the same SubjectID, form_name, feature_name, feature_unit and feature_delta, but different feature_value 
temp = data.all %>%
  count(SubjectID, form_name, feature_name, feature_unit, feature_delta) %>%
  filter(n > 1)
sum(temp$n) # 116,641 records 

temp = data.all %>%
  count(SubjectID, form_name, feature_name, feature_unit, feature_delta) %>%
  filter(n == 1) %>%
  select(-n)
dim(temp) # 4,301,089 records 

temp2 = data.all %>%
  inner_join(temp, by = c("SubjectID","form_name","feature_name","feature_unit","feature_delta"))

## Exclude records with feature_delta < 0 
dim(temp2) # 4,301,089 records 
temp3 = temp2 %>%
  filter(feature_delta < 0)
dim(temp3) # 39,827 records 

temp4 = temp2 %>%
  filter(is.na(feature_delta)) 

temp5 = temp2 %>%
  filter(feature_delta >= 0)

temp6 = rbind(temp4, temp5)
dim(temp6) # 4,261,262 records 

data.all = temp6
length(unique(data.all$SubjectID))

## Exclude patients with neither none or incomplete demographic information 
demographic = data.all %>% 
  filter(form_name == "Demographic") %>% 
  select(SubjectID, feature_name, feature_value)
length(unique(demographic$SubjectID)) # 8653 patients 
demographic = spread(demographic, feature_name, feature_value) 
demographic = within(demographic, {
  Age = round(as.numeric(Age))
  Gender = factor(Gender)
  Race = factor(Race)
})
demographic = demographic[complete.cases(demographic),]
length(unique(demographic$SubjectID)) # 8646 patients 
data.all_1 = data.all %>%
  filter(SubjectID %in% demographic$SubjectID)
dim(data.all_1) # 4,238,298

## Exclude patients with neither none or incomplete alshx data: onset_delta, diag_delta, onset_site
alshx = data.all_1 %>% 
  filter(form_name == "ALSHX") %>% 
  select(SubjectID, feature_name, feature_value) 
length(unique(alshx$SubjectID)) # 7,227 patients 
alshx = spread(alshx, feature_name, feature_value)
alshx = alshx[complete.cases(alshx),]
length(unique(alshx$SubjectID)) # 4,457 patients 

## Exclude records with errorneous diag_delta or onset_delta values 
alshx = within(alshx, {
  diag_delta = as.numeric(as.character(diag_delta))
  onset_delta = as.numeric(as.character(onset_delta))
  onset_site = factor(onset_site)
})
alshx = alshx %>%
  filter(diag_delta <= 0) %>%
  filter(onset_delta <= 0)
alshx = within(alshx, {
  diag_delta = -round((diag_delta/365)*12,2)
  onset_delta = -round((onset_delta/365)*12,2)
  onset2dx = onset_delta - diag_delta
})
## Exclude records with onset2dx < 0  
alshx = alshx %>%
  filter(onset2dx >= 0)
## onset_site: factor collapse 
alshx$onset_site = fct_collapse(alshx$onset_site, Nonbulbar = c("Limb","Limb and Bulbar","Other"))
length(unique(alshx$SubjectID)) # 4442 patients 

data.all_2 = data.all_1 %>%
  filter(SubjectID %in% alshx$SubjectID)
dim(data.all_2)

## ALSFRS
alsfrs <- data.all_2 %>%
  filter(form_name=="ALSFRS") %>%
  select(-c(form_name,feature_unit))
## Data type conversion  
alsfrs$feature_value <- as.numeric(alsfrs$feature_value)
alsfrs$feature_name = factor(alsfrs$feature_name)
## Convert long to wide format 
length(unique(alsfrs$SubjectID)) # 4082 patients 
alsfrs_wide <- spread(alsfrs,feature_name,feature_value)
dim(alsfrs_wide) # 37022 records 
length(unique(alsfrs_wide$SubjectID)) # 4082 patients 

## Exclude records with both Q5a and Q5b item scores recorded
temp = alsfrs_wide
temp2 = temp %>% 
  filter(!is.na(Q5a_Cutting_without_Gastrostomy)) %>%
  filter(!is.na(Q5b_Cutting_with_Gastrostomy)) %>%
  count(SubjectID, feature_delta) 
dim(temp2) # 14 records 
temp3 = temp %>%
  anti_join(temp2, by = c("SubjectID", "feature_delta")) 

## Create Gastrostomy field (TRUE/FALSE)  
temp4 = temp3 %>%
  mutate(Gastrostomy = ifelse(is.na(Q5b_Cutting_with_Gastrostomy), F, T)) %>%
  select(-c(Q5a_Cutting_without_Gastrostomy, Q5b_Cutting_with_Gastrostomy))
alsfrs_wide = temp4
length(unique(alsfrs_wide$SubjectID)) # 4082 patients 

## ALSFRS records 
## ALSFRS rev 
temp = alsfrs_wide %>%
  select(-c(ALSFRS_Total, 
            Q10_Respiratory, respiratory)) %>%
  mutate(motor = hands + leg + trunk) %>%
  rename(bulbar = mouth, respiratory = respiratory_R) %>%
  select(-c(hands, leg, trunk))
dim(temp) # 37,008 records, 4082 patients 

alsfrs_rev_wide = temp[complete.cases(temp),] 
dim(alsfrs_rev_wide) # 28,059 records 
length(unique(alsfrs_rev_wide$SubjectID)) # 3,059 patients 
alsfrs_rev_wide = alsfrs_rev_wide[c(1:3,5:16,4,19,17,18)]
#write.csv(alsfrs_rev_wide, "PROACT_preprocessed/ALSFRS_rev.csv", row.names = F, quote = F)


# Preprocess 2 for prediction 

data.all_3 = data.all_2 %>%
  filter(SubjectID %in% alsfrs_rev_wide$SubjectID)
dim(data.all_3) # 1,970,311 records 

## ALSFRS for the first 3 months 
alsfrs_rev_3mo = alsfrs_rev_wide %>%
  filter(feature_delta <= 3)
length(unique(alsfrs_rev_3mo$SubjectID)) # 3058 patients (1 of 3059 excluded)
length(unique(alsfrs_rev_wide$SubjectID)) # 3059 patients 

# Target event dataset 
tg = alsfrs_rev_wide %>%
  select(SubjectID, feature_delta, Q3_Swallowing)

# Patients with fu duration <= 3 mo (group I)
temp = tg %>%
  group_by(SubjectID) %>%
  summarise(last_delta = max(feature_delta)) %>%
  filter(last_delta <= 3)
dim(temp) # 299 patients (fu duration not greater than 3 mo)
# Patients in whom event occurred at or before 3 mo (group II)
temp2 = tg %>%
  filter(Q3_Swallowing <= 1) %>%
  group_by(SubjectID) %>%
  summarise(feature_delta = min(feature_delta)) %>%
  filter(feature_delta <= 3)
  # if event occurred, it should be after 3 mo 
dim(temp2) # 104 patients 

length(unique(union(temp$SubjectID, temp2$SubjectID))) # 388 patients 

# Exclude the above group I and II patients 
tg2 = tg %>%
  anti_join(temp, by = "SubjectID") %>%
  anti_join(temp2, by = "SubjectID")

length(unique(tg2$SubjectID)) # 2671 patients 

# target event dataset 
# event occurred 
tg_event = tg2 %>%
  filter(Q3_Swallowing <= 1) %>%
  group_by(SubjectID) %>%
  summarise(feature_delta = min(feature_delta)) %>%
  mutate(event = 1) %>%
  mutate(feature_delta = feature_delta - 3) # shift baseline to 3 mo
dim(tg_event) # 577 patients 
# censored 
tg_censored = tg2 %>%
  anti_join(tg_event, by = "SubjectID") %>%
  group_by(SubjectID) %>%
  summarise(feature_delta = max(feature_delta)) %>%
  mutate(feature_delta = feature_delta - 3) %>%
  mutate(event = 0)
dim(tg_censored) # 2094 patients 

tg_fin = rbind(tg_event, tg_censored)
write.csv(tg_fin, "~/Dropbox/ALSmaster/PROACT_data/PROACT_preprocessed/LOA_swallowing.csv", quote = F, row.names = F)

# Calculate meta-features of ALSFRS rev from records for the first 3 months: mean and slope 
# Select records for the first 3 months
alsfrs_rev_3mo_aggr = alsfrs_rev_3mo %>%
  group_by(SubjectID) %>%
  summarise(n=n(),
            first = min(feature_delta), 
            last = max(feature_delta), 
            interval = max(feature_delta) - min(feature_delta),
            Gastrostomy = ifelse(any(Gastrostomy == T), T, F),
            # Gastrostomy == T if patients underwent gastrostomy during the first 3 mo 
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

# Estimate ALSFRS total score slope with linear regression   
temp2_rev = alsfrs_rev_3mo_aggr %>%
  filter(interval >= 1.5) # select subjects with first-last interval >= 1.5 mo
temp3_rev = alsfrs_rev_3mo %>%
  filter(SubjectID %in% temp2_rev$SubjectID)
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
# Estimate slope_bulbar 
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
# Estimate slope_motor
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
# Estimate slope_respiratory
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

# Merge alsfrs_rev_3mo_aggr, alsfrs_rev_slope_total and alsfrs_rev_slope_bulbar and _motor and _respiratory 
alsfrs_rev_3mo_aggr_slope = alsfrs_rev_3mo_aggr %>%
  left_join(alsfrs_rev_slope_total, by = "SubjectID") %>%
  left_join(alsfrs_rev_slope_bulbar, by = "SubjectID") %>%
  left_join(alsfrs_rev_slope_motor, by = "SubjectID") %>%
  left_join(alsfrs_rev_slope_respiratory, by = "SubjectID")

alsfrs_rev_3mo_aggr_slope = alsfrs_rev_3mo_aggr_slope %>% 
  select(-c(n,first, last, interval)) 

# FVC
fvc = data.all_2 %>% # data.all_2: data (distinct, consistent) with full demographic and alshx (filtered)
  filter(form_name == "FVC") %>% 
  filter(feature_name == "fvc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta)
fvc$feature_value = round(as.numeric(fvc$feature_value))
fvc = fvc[complete.cases(fvc),]

# Calculate fvc_mean for the first 3 mo 
# Estimate fvc_slope with linear regression (3 mo)
fvc_3mo = fvc %>% 
  filter(feature_delta <= 3)
fvc_3mo_aggr = fvc_3mo %>%
  group_by(SubjectID) %>% 
  arrange(feature_delta) %>%
  summarise(fvc = mean(feature_value), 
            n=n(),
            first = min(feature_delta), 
            last = max(feature_delta), 
            interval = last(feature_delta) - first(feature_delta))
temp2 = fvc_3mo_aggr %>%
  filter(interval >= 1.5) # select subjects with interval >= 1.5 mo
length(unique(temp2$SubjectID)) # 2289 patients 
temp3 = fvc_3mo %>%
  filter(SubjectID %in% temp2$SubjectID) 
coef_fvc = temp3 %>% # Apply linear regression by SubjectID
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

# Family history 
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

# Vitals 
vitals = data.all_2 %>% filter(form_name == "Vitals") %>%
  select(SubjectID, feature_name, feature_value,  
         feature_delta)
vitals$feature_name = factor(vitals$feature_name)
vitals$feature_value = as.numeric(vitals$feature_value)

# BMI 
# Calculate mean BMI for the first 3 mo
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

# Vital sign: bp_diastolic, bp_systolic, pulse, respiratory_rate, temperature
vitalsign = vitals %>%
  filter(feature_name %in% 
           c("bp_systolic", "bp_diastolic", "pulse", 
             "respiratory_rate", "temperature"))
vitalsign_wide = spread(vitalsign, key = feature_name, value = feature_value)
dim(vitalsign_wide) # 31960 records 
length(unique(vitalsign_wide$SubjectID)) # 3623 patients 

temp = vitalsign_wide %>% 
  rename(dbp = bp_diastolic, 
       sbp = bp_systolic, 
       hr = pulse, 
       rr = respiratory_rate, 
       temp = temperature) -> temp

# Calculate mean vital sign over the first 3 mo  
vitalsign_3mo_aggr = temp %>%
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
length(unique(lab_wide$SubjectID)) # 3793 patients 
dim(lab_wide) # 32561 records 

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
merge_data = demographic %>%
  inner_join(alshx, by = "SubjectID") %>% 
  filter(SubjectID %in% alsfrs_rev_wide$SubjectID) %>%
  left_join(alsfrs_rev_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(fvc_3mo_aggr_slope, by = "SubjectID") %>%
  left_join(famhx, by = "SubjectID") %>%
  left_join(riluzole, by = "SubjectID") %>%
  left_join(bmi, by = "SubjectID") %>%
  left_join(vitalsign_3mo_aggr, by = "SubjectID") %>%
  left_join(lab_3mo_aggr, by = "SubjectID")

dim(merge_data)
length(unique(merge_data$SubjectID))

write.csv(merge_data, "~/Dropbox/ALSmaster/PROACT_data/PROACT_preprocessed/PROACT_features_predict.csv", quote = F, row.names = F)

# The End # 
