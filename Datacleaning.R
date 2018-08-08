setwd("/Users/hong/Documents/ALSMaster_data")

library(dplyr)
library(tidyr)

# Read raw data 

data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.allforms <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)

# Read preprocessed data (See Preprocess1.R and Preprocess2.R)

alsfrs_total = read.csv("ALSFRS_MITOS_all.csv")
alsfrs_sub = read.csv("ALSFRS_excl_leftcensored.csv")
subject_total = unique(alsfrs_total$SubjectID)
# subject_sub = unique(alsfrs_sub$SubjectID)

# Select target dataset 
data.allforms %>%
  filter(SubjectID %in% subject_total) -> data.all
# Type conversion; feature_delta, factor to numeric
data.all = within(data.all, {
  feature_delta = as.numeric(as.character(feature_delta))
  SubjectID = as.character(SubjectID)
})

# Demographic -> demographic.tab 로 정리
data.all %>% filter(form_name == "Demographic") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
demographic = spread(temp, feature_name, feature_value)
demographic = droplevels(demographic)
demographic$Age = as.numeric(as.character(demographic$Age))
demographic.tab = demographic[complete.cases(demographic),]
write.csv(demographic.tab, "Demographic.csv", quote = F, row.names = F)

# FVC -> fvc.tab 로 정리
# Extract data with feature_delta < 92 & >= 0
data.all %>% 
  filter(feature_delta < 92 & feature_delta >= 0) -> data.3mo
# Filter form_name == "FVC", feature_name =="fvc_percent"
data.3mo %>% filter(form_name == "FVC") %>% 
  select(SubjectID, feature_name, feature_value, feature_delta) -> fvc
fvc %>% filter(feature_name == "fvc_percent") -> fvc
fvc = droplevels(fvc)
# Type conversion 
fvc$feature_value = as.numeric(as.character(fvc$feature_value))
# Exclude data feature_value missing
fvc = fvc[!is.na(fvc$feature_value),]
# Exclude duplicated data
fvc = unique(fvc)
# Calculate fvc_mean, _min, _max 
fvc %>%
  group_by(SubjectID) %>% 
  summarise(fvc_mean = mean(feature_value), 
            fvc_min = min(feature_value), fvc_max = max(feature_value), 
            n=n()) -> fvc.meta
# Calculate fvc_slope with linear regression
# Exclude subjects with only one fvc datapoint 
subject.one.datapoint = fvc.meta[fvc.meta$n==1,]$SubjectID
fvc.sub = fvc[!(fvc$SubjectID %in% subject.one.datapoint),]
# Apply linear regression by SubjectID
lm = with(fvc.sub, by(fvc.sub, SubjectID, 
        function(x) lm(feature_value ~ feature_delta, data=x)))
fvc.slope = sapply(lm, coef)[2,]
slope.tab = as.data.frame(fvc.slope); slope.tab$SubjectID = rownames(slope.tab)
# Merge fvc.meta and slope.tab
merge(slope.tab, fvc.meta, by="SubjectID", all.y = T) -> fvc.tab
# Remove n (number of datapoints)
fvc.tab = fvc.tab[,1:5]
write.csv(fvc.tab, "FVC_meta.csv", quote=F, row.names = F)

# ALSFRS -> alsfrs.tab 로 정리 
# Use the preprocessed data (ALSFRS_R converted into ALSFRS)
alsfrs_total -> ac
ac %>% filter(feature_delta < 92 & feature_delta >= 0) -> ac.3mo

ac.3mo$SubjectID = as.character(ac.3mo$SubjectID)
ac.3mo %>%
  group_by(SubjectID) %>%
  summarize(n=n(), hands_mean = mean(hands),hands_min = min(hands), hands_max = max(hands),
            leg_mean = mean(leg), leg_min = min(leg), leg_max = max(leg),
            mouth_mean = mean(mouth), mouth_min = min(mouth), mouth_max = max(mouth),
            respiratory_mean = mean(respiratory), respiratory_min = min(respiratory), respiratory_max = max(respiratory),
            Q1_Speech_mean = mean(Q1_Speech), Q1_Speech_min = min(Q1_Speech), Q1_Speech_max = max(Q1_Speech),
            Q2_Salivation_mean = mean(Q2_Salivation), Q2_Salivation_min = min(Q2_Salivation), Q2_Salivation_max = max(Q2_Salivation),
            Q3_Swallowing_mean = mean(Q3_Swallowing), Q3_Swallowing_min = min(Q3_Swallowing), Q3_Swallowing_max = max(Q3_Swallowing),
            Q4_Handwriting_mean = mean(Q4_Handwriting), Q4_Handwriting_min = min(Q4_Handwriting), Q4_Handwriting_max = max(Q4_Handwriting),
            Q5_Cutting_mean = mean(Q5_Cutting), Q5_Cutting_min = min(Q5_Cutting), Q5_Cutting_max = max(Q5_Cutting),
            Q6_Dressing_and_Hygiene_mean = mean(Q6_Dressing_and_Hygiene), Q6_Dressing_and_Hygiene_min = min(Q6_Dressing_and_Hygiene), Q6_Dressing_and_Hygiene_max = max(Q6_Dressing_and_Hygiene),
            Q7_Turning_in_Bed_mean = mean(Q7_Turning_in_Bed), Q7_Turning_in_Bed_min = min(Q7_Turning_in_Bed), Q7_Turning_in_Bed_max = max(Q7_Turning_in_Bed),
            Q8_Walking_mean = mean(Q8_Walking), Q8_Walking_min = min(Q8_Walking), Q8_Walking_max = max(Q8_Walking),
            Q9_Climbing_Stairs_mean = mean(Q9_Climbing_Stairs), Q9_Climbing_Stairs_min = min(Q9_Climbing_Stairs), Q9_Climbing_Stairs_max = max(Q9_Climbing_Stairs),
            Q10R_mean = mean(Q10R), Q10R_min = min(Q10R), Q10R_max = max(Q10R),
            ALSFRS_Total_mean = mean(ALSFRS_Total), ALSFRS_Total_min = min(ALSFRS_Total), ALSFRS_Total_max = max(ALSFRS_Total),
            trunk_mean = mean(trunk), trunk_min = min(trunk), trunk_max = max(trunk)
            ) -> ac.meta
# Calculate alsfrs...slope with linear regression
# Exclude subjects with only one datapoint 
subject.one.datapoint = ac.meta[ac.meta$n==1,]$SubjectID
ac.sub = ac[!(ac$SubjectID %in% subject.one.datapoint),]

# Apply linear regression by SubjectID
###
slope = numeric()
for (i in 1:16) {
  lm = with(ac.sub, by(ac.sub, SubjectID, 
                       function(x) lm(as.formula(paste(names(ac.sub)[i+2], names(ac.sub)[2], 
                                                       sep="~")), data=x)))
  temp = sapply(lm, coef)[2,]
  slope = cbind(slope, temp)
  }
### 
colnames(slope) = paste(names(ac.sub)[3:18], "slope", sep="_")
slope = as.data.frame(slope)
slope$SubjectID = rownames(slope)
# Merge ac.meta and slope
ac.meta$SubjectID = as.character(ac.meta$SubjectID)
merge(slope, ac.meta, by="SubjectID", all.y = T) -> ac.tab
# Remove n (number of datapoints)
ac.tab %>% select(-n) -> alsfrs.tab
o1 = order(names(alsfrs.tab))
alsfrs.tab = alsfrs.tab[,o1]
alsfrs.tab = alsfrs.tab[,c(61,1:60,62:65)]
write.csv(alsfrs.tab, "ALSFRS_meta.csv", quote=F, row.names = F)

# ALSHX -> alshx.tab 로 정리 
# Filter form_name == "ALSHX"
data.all %>% filter(form_name == "ALSHX") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
alshx = spread(temp, feature_name, feature_value)
alshx = droplevels(alshx)
alshx.tab = within(alshx, {
               diag_delta = as.numeric(as.character(diag_delta))
               onset_delta = as.numeric(as.character(onset_delta))
               })
write.csv(alshx.tab, "ALShx.csv", quote=F, row.names=F)

# FamilyHx -> famhx
data.all %>% filter(form_name == "FamilyHx") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
famhx.tab = droplevels(spread(temp, feature_name, feature_value))
write.csv(famhx.tab, "FamilyHx.csv", quote=F, row.names=F)

# Riluzole
data.all %>% filter(form_name == "Riluzole") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
riluzole.tab = droplevels(spread(temp, feature_name, feature_value))
write.csv(riluzole.tab, "Riluzole.csv", quote=F, row.names=F)

# SVC
data.3mo %>% filter(form_name == "SVC") %>% 
  filter(feature_name == "svc_percent") %>%
  select(SubjectID, feature_name, feature_value, feature_delta) -> temp
svc = droplevels(temp)
# Type conversion 
svc$feature_value = as.numeric(as.character(svc$feature_value))
# Calculate svc_mean, _min, _max 
svc %>%
  group_by(SubjectID) %>% 
  summarise(svc_mean = mean(feature_value), 
            svc_min = min(feature_value), svc_max = max(feature_value), 
            n=n()) -> svc.meta
# Calculate svc_slope with linear regression
# Exclude subjects with only one svc datapoint 
subject.one.datapoint = svc.meta[svc.meta$n==1,]$SubjectID
svc.sub = svc[!(svc$SubjectID %in% subject.one.datapoint),]
# Apply linear regression by SubjectID
lm = with(svc.sub, by(svc.sub, SubjectID, 
                      function(x) lm(feature_value ~ feature_delta, data=x)))
svc.slope = sapply(lm, coef)[2,]
slope.svc = as.data.frame(svc.slope); slope.svc$SubjectID = rownames(slope.svc)
# Merge fvc.meta and slope.tab
merge(slope.svc, svc.meta, by="SubjectID", all.y = T) -> svc.tab
# Remove n (number of datapoints)
svc.tab = svc.tab[,1:5]
write.csv(svc.tab, "SVC_meta.csv", quote=F, row.names = F)

# Treatment 
data.all %>% filter(form_name == "Treatment") -> temp
tx = spread(temp, feature_name, feature_value)
tx = droplevels(tx)
tx %>% select(SubjectID, treatment_group, feature_delta) -> tx.tab
colnames(tx.tab)[3] = "treatment_delta"
write.csv(tx.tab, "Treatment.csv", quote = F, row.names = F)

# Vitals : vitals.tab 으로 정리 
data.3mo %>% filter(form_name == "Vitals") -> temp
temp = droplevels(temp)
vitals = unique(temp)
vitals$feature_value = as.numeric(as.character(vitals$feature_value))
# Average duplicate values 
vitals %>%
  group_by(SubjectID, feature_name) %>%
  summarise(feature_value=mean(feature_value,na.rm=TRUE)) -> temp
# Convert long to wide format 
vitals.tab = spread(temp, feature_name, feature_value)
vitals.tab$BMI = vitals.tab$BMI*10000
write.csv(vitals.tab, "Vitals.csv", quote=F, row.names = F)

# Lab Test : lab.tab 으로 정리 
data.3mo %>% filter(form_name == "Lab Test") -> temp
lab.temp = droplevels(temp)
# Phosphorous, Creatinine, Uric Acid, CK, etc 
# levels(lab.temp$feature_name)
# lab.temp %>% filter(feature_name == "Triglycerides") -> temp
# temp = droplevels(temp)
# temp$feature_value = as.numeric(as.character(temp$feature_value))
# temp = temp[!is.na(temp$feature_value),]
# str(temp)
# plot(density(temp$feature_value))

lab.temp %>% 
  filter(feature_name %in% 
           c("Absolute Neutrophil Count","Absolute Lymphocyte Count","Albumin",
             "Bicarbonate","Blood Urea Nitrogen (BUN)",
             "Calcium","Chloride","CK","Creatinine","GFR",
             "Phosphorus","Sodium","Total Cholesterol","Triglycerides",
             "Uric Acid","Urine Creatinine",
             "Urine Creatinine Clearance","Urine Ph"
             )) -> lab

# Exclude some features 
# C-Reactive Protein <- feature_value: ..., Normal, ... 

lab = droplevels(lab)
lab$feature_value = as.numeric(as.character(lab$feature_value))
lab = lab[!is.na(lab$feature_value),]
lab %>%
  group_by(SubjectID, feature_name) %>%
  summarise(feature_value = mean(feature_value)) -> lab.tab
lab.tab = spread(lab.tab, feature_name, feature_value)

write.csv(lab.tab, "Lab.csv", quote=F, row.names = F)

# Final data merging 
data.processed = list()
data.processed[[1]] = demographic.tab
data.processed[[2]] = fvc.tab
data.processed[[3]] = alsfrs.tab
data.processed[[4]] = alshx.tab
data.processed[[5]] = famhx.tab
data.processed[[6]] = riluzole.tab
data.processed[[7]] = svc.tab
data.processed[[8]] = tx.tab
data.processed[[9]] = vitals.tab
data.processed[[10]] = lab.tab

temp = data.processed[[1]]

for (i in 1:9){
  temp = merge(temp, data.processed[[i+1]], by="SubjectID", all=T)
}

data.fin = temp

write.csv(data.fin, "PROACT_preprocessed_cleaned.csv", quote=F, row.names=F)


