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

# ALSFRS_slope datasets 
slope_training = read.delim("ALSFRS_slope_PROACT_training.txt", sep = "|", header = T)
slope_training2 = read.delim("ALSFRS_slope_PROACT_training2.txt", sep = "|", header = T)
slope_validation = read.delim("ALSFRS_slope_PROACT_validation.txt", sep = "|", header = T)
slope_leaderboard = read.delim("ALSFRS_slope_PROACT_leaderboard.txt", sep = "|", header = T)
slope_all = rbind(slope_training, slope_training2, slope_leaderboard, slope_validation)
length(unique(slope_all$SubjectID)) # 3096 patients 
slope_all$SubjectID <- as.character(slope_all$SubjectID)

write.csv(slope_all, "ALSFRS_slope.csv", row.names = F, quote = F)

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
# Exclude alsfrs records with feature_delta < 0 
range(alsfrs$feature_delta)
alsfrs = alsfrs %>%
  filter(feature_delta >= 0) 
# excluded records with feature_delta < 0 
# 507 records in 27 patients 

# Exclude duplicates
dim(alsfrs)
temp = alsfrs
temp = unique(temp) 
# exclude records that are completely the same
dim(temp)
# Exclude ALSFRS records with the same feature_delta 
# but having different feature_value
temp$id_delta = paste(temp$SubjectID, 
                      temp$feature_delta, sep = "_")
temp2 = temp %>%
  select(-feature_value)
temp3 = temp2[duplicated(temp2),]
duplicated_id_delta = unique(temp3$id_delta)
temp4 = temp %>%
  filter(!(id_delta %in% duplicated_id_delta)) %>%
  select(-id_delta)
temp4$feature_name = factor(temp4$feature_name)

alsfrs_wide <- spread(temp4,feature_name,feature_value)
dim(alsfrs_wide) #59,269 records 


# ALSFRS original vs. revised records 
orig = alsfrs_wide[!is.na(alsfrs_wide$ALSFRS_Total),]$SubjectID
orig_id = unique(orig)
rev = alsfrs_wide[!is.na(alsfrs_wide$ALSFRS_R_Total),]$SubjectID
rev_id = unique(rev)
length(intersect(orig_id, rev_id)) # 3,412 patients 
temp = alsfrs_wide %>%
  filter(!is.na(ALSFRS_Total)) %>%
  filter(!is.na(ALSFRS_R_Total))
dim(temp) 
# 30,154 records with both ALSFRS original and revised total scores 

# ALSFRS original 
alsfrs_original_wide = alsfrs_wide %>%
  filter(!is.na(ALSFRS_Total)) %>%
  select(-c(ALSFRS_R_Total, 
            R1_Dyspnea, R2_Orthopnea, 
            R3_Respiratory_Insufficiency, 
            respiratory_R))
dim(alsfrs_original_wide) # 59,260 records 
length(unique(alsfrs_original_wide$SubjectID)) # 6,510 patients 
table(table(alsfrs_original_wide$SubjectID))
range(alsfrs_original_wide$feature_delta)

temp = alsfrs_original_wide
summary(temp) # NA in Q10_Respiratory: 23063 records 
# probably error in filling in the respiratory dimension score...
# used respiratory dimension score for Q10_Respiratory item score in ALSFRS original version scoring 
temp$Q10_Respiratory = ifelse(is.na(temp$Q10_Respiratory), 
                              temp$respiratory, 
                              temp$Q10_Respiratory) 
alsfrs_original_wide = temp
alsfrs_original_wide = 
  alsfrs_original_wide[,c(1,2,3,7,9:18,8,6,4,5,20,19)]

# ALSFRS revised 
alsfrs_revised_wide = alsfrs_wide %>%
  filter(!is.na(ALSFRS_R_Total)) %>%
  select(-c(ALSFRS_Total, 
            Q10_Respiratory, respiratory))
dim(alsfrs_revised_wide) # 30,167 records 
length(unique(alsfrs_revised_wide$SubjectID)) # 3,412 patients 
table(table(alsfrs_revised_wide$SubjectID))
alsfrs_revised_wide = 
  alsfrs_revised_wide[,c(1,2,3,7:20,6,4,5,22,21)]

write.csv(alsfrs_original_wide, "ALSFRS_original.csv",
          row.names = F, quote = F)
write.csv(alsfrs_revised_wide, "ALSFRS_revised.csv",
          row.names = F, quote = F)

# ALSFRS original vs. revised 
# Select records for the first 3 months
# Meta-features: mean, min, max and linear regression slope 

alsfrs_3mo = alsfrs_original_wide %>%
  filter(((feature_delta)/365)*12 <= 3)
range(alsfrs_3mo$feature_delta)
length(unique(alsfrs_3mo$SubjectID)) # 6507 patients 
table(table(alsfrs_3mo$SubjectID)) # 340 patients have only one record
dim(alsfrs_3mo) # 20,841 records 

# create meta feature; gastrostomy
alsfrs_3mo <- alsfrs_3mo %>%
  mutate(Gastrostomy = ifelse(is.na(Q5b_Cutting_with_Gastrostomy), F, T)) %>%
  mutate(Gastrostomy = ifelse(is.na(Q5a_Cutting_without_Gastrostomy) & 
                       is.na(Q5b_Cutting_with_Gastrostomy), NA, Gastrostomy))

alsfrs_3mo = alsfrs_3mo %>%
  filter(!is.na(Gastrostomy)) 
# excluded 10 records with missing data in Q5_Cutting

gastrostomy = alsfrs_3mo %>%
  select(SubjectID, Gastrostomy)
dim(gastrostomy)
# some patients underwent gastrostomy during the first 3 mo
gastrostomy = unique(gastrostomy)
dim(gastrostomy) # 6667 records 
length(unique(gastrostomy$SubjectID))
# gastrostomy == T if patients underwent gastrostomy during the first 3 mo 
temp = gastrostomy %>% 
  group_by(SubjectID) %>%
  summarise(Gastrostomy = ifelse(any(Gastrostomy == T), T, F)) 
gastrostomy = temp

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
alsfrs_3mo_meta = merge(alsfrs_3mo_aggr, gastrostomy, by = "SubjectID")
summary(alsfrs_3mo_meta)

# estimate alsfrs slope with linear regression, 
# in those subjects with two or more datapoints during the first 3 months 
temp = alsfrs_3mo_meta
subject.one.datapoint = temp[temp$n==1,]$SubjectID
temp2 = alsfrs_3mo[!(alsfrs_3mo$SubjectID %in% 
                             subject.one.datapoint),]
# Apply linear regression by SubjectID
lm = with(temp2, by(temp2, SubjectID, 
                       function(x) lm(ALSFRS_Total ~ feature_delta, 
                                      data=x)))
alsfrs_slope = sapply(lm, coef)[2,]
alsfrs_slope_tab = data.frame(alsfrs_slope)
alsfrs_slope_tab$SubjectID = rownames(alsfrs_slope_tab)
plot(density(alsfrs_slope_tab$alsfrs_slope))

# Merge fvc.meta and slope.tab
alsfrs_3mo_meta_slope = merge(alsfrs_slope_tab, 
                               alsfrs_3mo_meta, 
                               by="SubjectID", all.y = T) 
alsfrs_3mo_meta_slope = alsfrs_3mo_meta_slope %>%
  select(-c(n, first, last, interval))

summary(alsfrs_3mo_meta_slope)

write.csv(alsfrs_3mo_meta_slope, "alsfrs_orig_3mo_meta_slope.csv", 
          quote=F, row.names = F)

df = merge(demographic, alsfrs_3mo_meta_slope, by = "SubjectID", all.x = T)

# # ALSFRS_R_Total meta-features and slope during the first 3 mo 
# alsfrs_r_total_3mo = alsfrs_revised_wide %>%
#   filter(feature_delta >=0 & feature_delta < 92) %>%
#   select(SubjectID, ALSFRS_R_Total, feature_delta)
# 
# alsfrs_r_total_3mo_meta = alsfrs_r_total_3mo %>%
#   group_by(SubjectID) %>% 
#   summarise(mean_alsfrs_r_total = mean(ALSFRS_R_Total), 
#             min_alsfrs_r_total = min(ALSFRS_R_Total), 
#             max_alsfrs_r_total = max(ALSFRS_R_Total), 
#             n=n(),
#             first = first(feature_delta), 
#             last = last(feature_delta), 
#             interval = last(feature_delta) - first(feature_delta))
# dim(alsfrs_r_total_3mo_meta)
# 
# # estimate alsfrs slope with linear regression, 
# # excluding subjects with only one datapoint 
# temp = alsfrs_r_total_3mo_meta
# subject.one.datapoint = temp[temp$n==1,]$SubjectID
# temp2 = alsfrs_r_total_3mo[!(alsfrs_r_total_3mo$SubjectID %in% 
#                              subject.one.datapoint),]
# 
# # Apply linear regression by SubjectID
# ###
# lm = with(temp2, by(temp2, SubjectID, 
#                     function(x) lm(ALSFRS_R_Total ~ feature_delta, 
#                                    data=x)))
# 
# alsfrs_r_total_slope = sapply(lm, coef)[2,]
# alsfrs_r_total_slope_tab = as.data.frame(alsfrs_r_total_slope)
# alsfrs_r_total_slope_tab$SubjectID = rownames(alsfrs_r_total_slope_tab)
# # Merge fvc.meta and slope.tab
# alsfrs_r_total_3mo_meta_slope = merge(alsfrs_r_total_slope_tab, 
#                                     alsfrs_r_total_3mo_meta, 
#                                     by="SubjectID", all.y = T)
# write.csv(alsfrs_r_total_3mo_meta_slope, "alsfrs_r_total_3mo_meta_slope.csv", 
#           quote=F, row.names = F)


# FVC & SVC  
# FVC
fvc = data.all %>% 
  filter(form_name == "FVC") %>% 
  filter(feature_name == "fvc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta)
range(fvc$feature_delta)
fvc$feature_value = round(as.numeric(fvc$feature_value))
fvc = fvc %>%
  filter(feature_delta >= 0)
# exclude duplicate records 
fvc = unique(fvc)
fvc$id_delta = paste(fvc$SubjectID, fvc$feature_delta, 
                     sep = "_")
temp = fvc %>%
  select(-feature_value)
duplicated_id_delta = unique(temp[duplicated(temp),]$id_delta) 
length(duplicated_id_delta) # 41 records 
fvc = fvc %>%
  filter(!(id_delta %in% duplicated_id_delta)) %>%
  select(-c(id_delta, feature_name))
names(fvc)[2] = "fvc_percent"
fvc = fvc %>%
  filter(!is.na(fvc_percent))
dim(fvc) # 44516 records 
length(unique(fvc$SubjectID)) # 7313 patients 
range(fvc$feature_delta)

write.csv(fvc, "fvc.csv", row.names = F, quote = F)

# FVC_3mo 
# Extract data with feature_delta < 92
fvc_3mo = fvc %>% 
  filter((feature_delta/365)*12 <= 3)
length(unique(fvc_3mo$SubjectID)) # 7767 patients 
dim(fvc_3mo)

# Calculate fvc_mean, _min, _max 
fvc_3mo_meta = fvc_3mo %>%
  group_by(SubjectID) %>% 
  summarise(fvc_mean = mean(fvc_percent), 
            fvc_min = min(fvc_percent), 
            fvc_max = max(fvc_percent), 
            n=n(),
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta))
dim(fvc_3mo_meta)

# Calculate fvc_slope with linear regression, 
# excluding subjects with only one fvc datapoint 
subject.one.datapoint = fvc_3mo_meta[fvc_3mo_meta$n==1,]$SubjectID
length(subject.one.datapoint) # 1962 patients 
fvc_3mo_sub = fvc_3mo[!(fvc_3mo$SubjectID %in% subject.one.datapoint),]

# Apply linear regression by SubjectID
lm = with(fvc_3mo_sub, by(fvc_3mo_sub, SubjectID, 
                          function(x) lm(fvc_percent ~ feature_delta, data=x)))
fvc_slope = sapply(lm, coef)[2,]
slope.tab = data.frame(fvc_slope)
slope.tab$SubjectID = rownames(slope.tab)
plot(density(slope.tab$fvc_slope))

# Merge fvc.meta and slope.tab
fvc.tab = merge(slope.tab, fvc_3mo_meta, by="SubjectID", all.y = T)
fvc.tab = fvc.tab %>%
  select(-c(n, first, last, interval))

write.csv(fvc.tab, "fvc_3mo_meta.csv", quote=F, row.names = F)

df2 = merge(df, fvc.tab, by="SubjectID", all.x = T)

# SVC 
svc = data.all %>% 
  filter(form_name == "SVC") %>% 
  filter(feature_name == "svc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta)
svc$feature_value = round(as.numeric(svc$feature_value))
# exclude dupldates 
svc = unique(svc)
svc$id_delta = paste(svc$SubjectID, svc$feature_delta, sep = "_")
temp = svc %>%
  select(-feature_value)
duplicated_id_delta = temp[duplicated(temp),]$id_delta
temp2 = svc %>%
  filter(!(id_delta %in% duplicated_id_delta)) %>%
  select(-c(id_delta, feature_name))
names(temp2)[2] = "svc_percent"
svc = temp2 
# excluded records 

range(svc$feature_delta)
length(unique(svc$SubjectID)) # 695 patients 
dim(svc) # 4826 records 
table(table(svc$SubjectID))

write.csv(svc, "svc.csv", row.names = F, quote = F)

# SVC_3mo 
# Extract data with feature_delta < 92
svc_3mo = svc %>% 
  filter((feature_delta/365)*12 <= 3)
length(unique(svc_3mo$SubjectID)) # 695 patients
dim(svc_3mo) # 2056 records 

# Calculate svc_mean, _min, _max 
svc_3mo_meta = svc_3mo %>%
  group_by(SubjectID) %>% 
  summarise(svc_mean = mean(svc_percent), 
            svc_min = min(svc_percent), 
            svc_max = max(svc_percent), 
            n=n(), 
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta))
dim(svc_3mo_meta) 

# Calculate svc_slope with linear regression, 
# excluding subjects with only one svc datapoint 
subject.one.datapoint = svc_3mo_meta[svc_3mo_meta$n==1,]$SubjectID
svc_3mo_sub = svc_3mo[!(svc_3mo$SubjectID %in% subject.one.datapoint),]

# Apply linear regression by SubjectID
lm = with(svc_3mo_sub, by(svc_3mo_sub, SubjectID, 
                          function(x) lm(svc_percent ~ feature_delta, data=x)))
svc_slope = sapply(lm, coef)[2,]
slope.tab = as.data.frame(svc_slope)
slope.tab$SubjectID = rownames(slope.tab)
# Merge svc.meta and slope.tab
svc.tab = merge(slope.tab, svc_3mo_meta, by="SubjectID", all.y = T)
svc.tab = svc.tab %>%
  select(-c(n, first, last, interval))

write.csv(svc.tab, "svc_3mo_meta.csv", quote=F, row.names = F)

df3 = merge(df2, svc.tab, by = "SubjectID", all.x = T)

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
dim(alshx) # 4454 patients 
summary(alshx)
write.csv(alshx, "als_hx.csv", quote=F, row.names=F)

df4 = merge(df3, alshx, by="SubjectID", all.x = T)

# FamilyHx -> famhx
data.all %>% filter(form_name == "FamilyHx") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
temp$feature_name = factor(temp$feature_name)
temp$feature_value = factor(temp$feature_value)

famhx = spread(temp, feature_name, feature_value)
dim(famhx) # 1007 patients
write.csv(famhx, "family_hx.csv", quote=F, row.names=F)

df5 = merge(df4, famhx, by="SubjectID", all.x = T)

# Riluzole
data.all %>% filter(form_name == "Riluzole") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
temp$feature_name = factor(temp$feature_name)
temp$feature_value = factor(temp$feature_value)
riluzole = spread(temp, feature_name, feature_value)
dim(riluzole) # 8817 patients
write.csv(riluzole, "riluzole.csv", quote=F, row.names=F)

df6 = merge(df5, riluzole, by = "SubjectID", all.x = T)

# Treatment group: active vs. placebo 
data.all %>% filter(form_name == "Treatment") %>%
  select(SubjectID, feature_name, feature_value) -> temp
tx = spread(temp, feature_name, feature_value)
tx$treatment_group = factor(tx$treatment_group)
dim(tx) # 9640 patients 
write.csv(tx, "treatment_group.csv", quote = F, row.names = F)

df7 = merge(df6, tx, by = "SubjectID", all.x = T)

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
temp$id_delta = paste(temp$SubjectID, temp$feature_delta, 
                      sep = "_")
temp$id_feature_delta = paste(temp$id_delta, temp$feature_name, 
                              sep = "_")
temp2 = temp %>%
  select(-feature_value)
# exclude records with the same SubjectID, 
# feature_name and feature_delta but different feature_value
duplicated_id_feature_delta = temp2[duplicated(temp2),]$id_feature_delta
temp3 = temp %>% 
  filter(!(id_feature_delta %in% duplicated_id_feature_delta)) %>%
  select(-c(id_delta, id_feature_delta))
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
bmi_3mo = bmi_wide %>%
  filter((feature_delta/365)*12 <= 3) %>%
  group_by(SubjectID) %>% 
  summarise(BMI_3mo_mean = mean(BMI))
dim(bmi_3mo) 

df8 = merge(df7, bmi_3mo, by = "SubjectID", all.x = T)

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
wt_3mo = wt_wide %>%
  filter((feature_delta/365)*12 <= 3) %>%
  group_by(SubjectID) %>% 
  summarise(Wt_3mo_mean = mean(weight))
dim(bmi_3mo) # 4549 patients 

df9 = merge(df8, wt_3mo, by = "SubjectID", all.x = T)

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
dim(vitalsign_wide) # 61261 records 
length(unique(vitalsign_wide$SubjectID)) # 7044 patients 

write.csv(vitalsign_wide, "vitalsign.csv", quote=F, row.names = F)

# Calculate mean 
vitalsign_3mo = vitalsign_wide %>%
  filter((feature_delta/365)*12 <= 3) %>%
  group_by(SubjectID) %>% 
  summarise(bp_diastolic_3mo_mean = mean(bp_diastolic), 
            bp_systolic_3mo_mean = mean(bp_systolic), 
            pulse_3mo_mean = mean(pulse), 
            respiratory_rate_3mo_mean = mean(respiratory_rate),
            temperature_3mo_mean = mean(temperature))
dim(vitalsign_3mo) # 7024 patients

df10 = merge(df9, vitalsign_3mo, by = "SubjectID", all.x = T)

# Lab 
lab = data.all %>% filter(form_name == "Lab Test") %>%
  select(-c(form_name, feature_unit))
lab$feature_name = factor(lab$feature_name)
levels(lab$feature_name)
lab$feature_value = as.numeric(lab$feature_value)
lab = lab %>%
  filter(feature_delta >= 0)
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
# exclude duplicates 
lab = unique(lab)
lab_sub -> temp
temp$id_feature_delta = paste(
  paste(temp$SubjectID, temp$feature_name, sep = "_"), 
  temp$feature_delta, sep = "_)")
temp2 = temp %>%
  select(-feature_value)
duplicated_id_feature_delta = temp2[duplicated(temp2),]$id_feature_delta
temp3 = temp %>%
  filter(!(id_feature_delta %in% duplicated_id_feature_delta))
lab_sub = temp3 %>%
  select(-id_feature_delta)

lab.tab = spread(lab_sub, feature_name, feature_value)
range(lab.tab$feature_delta)
length(unique(lab.tab$SubjectID)) # 7775 patients 
dim(lab.tab) # 66256 records 

write.csv(lab.tab, "lab.csv", quote=F, row.names = F)

# Calculate mean 
lab_3mo = lab.tab %>%
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

dim(lab_3mo) # 7709 patients
summary(lab_3mo)

df11 = merge(df10, lab_3mo, by = "SubjectID", all.x = T)

write.csv(df11, "PROACT_preprocessed.csv", quote = F, row.names = F)

# The End # 
