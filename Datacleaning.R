setwd("/Users/hong/Documents/ALSMaster_data")

library(dplyr)
library(tidyr)

# Read raw data 
data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.allforms <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)

data.all = data.allforms

# Type conversion; feature_delta, factor to numeric
data.all$SubjectID = as.character(data.all$SubjectID)
data.all$feature_delta = as.numeric(data.all$feature_delta)
data.all$form_name = factor(data.all$form_name)
levels(data.all$form_name)

# FVC & SVC  
# FVC
fvc = data.all %>% 
  filter(form_name == "FVC") %>% 
  filter(feature_name == "fvc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta)
fvc = unique(fvc)
fvc$feature_value = round(as.numeric(fvc$feature_value))
fvc$feature_delta = as.numeric(fvc$feature_value)

fvc$id_delta = paste(fvc$SubjectID, fvc$feature_delta, 
                     sep = "_")
temp = fvc %>%
  select(-feature_value)
duplicated_id_delta = temp[duplicated(temp),]$id_delta
temp2 = fvc %>%
  filter(!(id_delta %in% duplicated_id_delta)) %>%
  select(-c(id_delta, feature_name))
names(temp2)[2] = "fvc_percent"
fvc = temp2 
fvc = fvc[complete.cases(fvc),]
range(fvc$feature_delta)

length(unique(fvc$SubjectID)) # 7269 patients 
table(table(fvc$SubjectID))

write.csv(fvc, "fvc.csv", row.names = F, quote = F)

# SVC 
svc = data.all %>% 
  filter(form_name == "SVC") %>% 
  filter(feature_name == "svc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta)
svc$feature_value = round(as.numeric(svc$feature_value))
svc$feature_delta = as.numeric(svc$feature_value)
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
svc = svc[complete.cases(svc),]
range(svc$feature_delta)

length(unique(svc$SubjectID)) # 694 patients 
table(table(svc$SubjectID))

write.csv(svc, "svc.csv", row.names = F, quote = F)

# FVC_3mo 
# Extract data with feature_delta < 92
fvc_3mo = fvc %>% 
  filter(feature_delta >= 0 & feature_delta < 92)
length(unique(fvc_3mo$SubjectID)) # 6303

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
fvc_3mo_sub = fvc_3mo[!(fvc_3mo$SubjectID %in% subject.one.datapoint),]

# Apply linear regression by SubjectID
lm = with(fvc_3mo_sub, by(fvc_3mo_sub, SubjectID, 
        function(x) lm(fvc_percent ~ feature_delta, data=x)))
fvc_slope = sapply(lm, coef)[2,]
slope.tab = as.data.frame(fvc_slope)
slope.tab$SubjectID = rownames(slope.tab)
# Merge fvc.meta and slope.tab
fvc.tab = merge(slope.tab, fvc_3mo_meta, by="SubjectID", all.y = T)
write.csv(fvc.tab, "fvc_3mo_meta.csv", quote=F, row.names = F)

# SVC_3mo 
# Extract data with feature_delta < 92
svc_3mo = svc %>% 
  filter(feature_delta >= 0 & feature_delta < 92)
length(unique(svc_3mo$SubjectID)) # 625 patients

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
write.csv(svc.tab, "svc_3mo_meta.csv", quote=F, row.names = F)


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
write.csv(alshx, "als_hx.csv", quote=F, row.names=F)

# FamilyHx -> famhx
data.all %>% filter(form_name == "FamilyHx") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
temp$feature_name = factor(temp$feature_name)
temp$feature_value = factor(temp$feature_value)

famhx = spread(temp, feature_name, feature_value)
dim(famhx) # 1007 patients
write.csv(famhx, "family_hx.csv", quote=F, row.names=F)

# Riluzole
data.all %>% filter(form_name == "Riluzole") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
temp$feature_name = factor(temp$feature_name)
temp$feature_value = factor(temp$feature_value)
riluzole = spread(temp, feature_name, feature_value)
dim(riluzole) # 8817 patients
write.csv(riluzole, "riluzole.csv", quote=F, row.names=F)

# Treatment group: active vs. placebo 
data.all %>% filter(form_name == "Treatment") %>%
  select(SubjectID, feature_name, feature_value) -> temp
tx = spread(temp, feature_name, feature_value)
tx$treatment_group = factor(tx$treatment_group)
dim(tx) # 9640 patients 
write.csv(tx, "treatment_group.csv", quote = F, row.names = F)

# Vitals 
data.all %>% filter(form_name == "Vitals") %>%
  select(SubjectID, feature_name, feature_value,  
         feature_delta) -> temp
temp$feature_name = factor(temp$feature_name)
levels(temp$feature_name)
temp2 = unique(temp) # exclude duplicates
temp2$feature_value = as.numeric(temp2$feature_value)
temp2$feature_delta = as.numeric(temp2$feature_delta)
vitals = temp2

temp = vitals 
temp$id_delta = paste(temp$SubjectID, temp$feature_delta, 
                      sep = "_")
temp$id_feature_delta = paste(temp$id_delta, temp$feature_name, 
                              sep = "_")
temp2 = temp %>%
  select(-feature_value)

duplicated_id_feature_delta = temp2[duplicated(temp2),]$id_feature_delta
temp3 = temp %>% # exclude records with the same SubjectID, feature_name and feature_delta but different feature_value
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

# Weight 
wt = vitals %>%
  filter(feature_name  == "weight") 

wt_wide = spread(wt, key = feature_name, value = feature_value)
dim(wt_wide) # 50,281
length(unique(wt_wide$SubjectID)) # 7588
table(table(wt_wide$SubjectID))

wt_wide = wt_wide[,c(1,3,2)]
range(wt_wide$feature_delta, na.rm = T)

write.csv(wt_wide, "weight.csv", quote = F, row.names = F)

# bp_diastolic, bp_systolic, pulse, respiratory_rate, temperature
vitals = vitals %>%
  filter(feature_name %in% 
           c("bp_systolic", "bp_diastolic", "pulse", 
             "respiratory_rate", "temperature"))

vitals_wide = spread(vitals, key = feature_name, value = feature_value)
dim(vitals_wide) # 61261 records 
length(unique(vitals_wide$SubjectID)) # 7044 patients 

write.csv(vitals_wide, "vitals.csv", quote=F, row.names = F)

# Lab 
lab = data.all %>% filter(form_name == "Lab Test") %>%
  select(-c(form_name, feature_unit))
lab$feature_name = factor(lab$feature_name)
lab$feature_delta = as.numeric(lab$feature_delta)
lab = unique(lab)
lab = lab %>%
  filter(!is.na(feature_delta))
lab_sub = lab %>% 
  filter(feature_name %in% 
           c("Absolute Neutrophil Count",
             "Absolute Lymphocyte Count",
             "Absolute Monocyte Count",
             "C-Reactive Protein",
             "CK",
             "Creatinine",
             "GFR",
             "Total Cholesterol",
             "Triglycerides",
             "HDL",
             "LDL",
             "Uric Acid",
             "Urine Creatinine",
             "Urine Creatinine Clearance"
             ))

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
length(unique(lab.tab$SubjectID)) # 7840
dim(lab.tab)
table(table(lab.tab$SubjectID))

write.csv(lab.tab, "lab.csv", quote=F, row.names = F)

# The End # 
