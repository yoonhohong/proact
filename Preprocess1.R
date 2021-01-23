# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# Load packages
library(dplyr)
library(tidyr)
library(survival)

# 데이터 read
data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.allforms <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)
length(unique(data.allforms$SubjectID)) # 10723 patients

# For making a material for data medicine course in 2018...
# set.seed(1)
# data.allforms %>%
#   filter(!(form_name %in% c("Lab Test", "Adverse Event",
#                             "Concomitant Medication"))) -> Others
# 
# data.allforms %>%
#   filter((form_name == "Lab Test")) -> Lab
# Lab %>%
#   filter(feature_name %in% c("Absolute Band Neutrophil Count",
#                              "Absolute Neutrophil Count",
#                              "Absolute Segmented Neutrophil Count",
#                              "Absolute Lymphocyte count",
#                              "Absolute Lymphocyte Count",
#                              "Lymphocytes",
#                              "Neutrophils",
#                              "C-Reactive Protein",
#                              "CK",
#                              "Creatinine",
#                              "Uric Acid")) -> Lab.sub
# 
# df = rbind(Lab.sub, Others)
# sampl = sample(unique(df$SubjectID), 2000)
# 
# df.sampl = df %>%
#   filter(SubjectID %in% sampl)
# df.sampl = droplevels(df.sampl)
# write.table(df.sampl, file = "proact_sample.txt", quote = F, sep="|", row.names = F, col.names = T)

# Demographic
data.allforms %>% filter(form_name == "Demographic") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
demographic = spread(temp, feature_name, feature_value)
demographic_full = demographic[complete.cases(demographic),]
# Number of subjects with complete demographic information (age, gender, race)
length(unique(demographic_full$SubjectID)) # 8,646 patients
# Extract subjects with full demographic data
data.allforms %>% filter(SubjectID %in% unique(demographic_full$SubjectID)) -> data.all

# ALSFRS form만 불러오기
alsfrs <- droplevels(data.all[data.all$form_name=="ALSFRS", ])
alsfrs = subset(alsfrs,select=-c(form_name,feature_unit))
alsfrs$feature_value <- as.numeric(as.character(alsfrs$feature_value))
alsfrs$feature_delta <- as.numeric(as.character(alsfrs$feature_delta))
# ALSFRS에서 같은 환자가 같은 시점에 2번 측정한 데이터 처리 by mean
alsfrs %>%
  group_by(SubjectID, feature_name, feature_delta) %>%
  summarise(feature_value=mean(feature_value,na.rm=TRUE)) -> temp
# Wide format 형태로 변환
alsfrs_wide <- spread(temp,feature_name,feature_value)
# Explore: pateints with at least one ALSFRS or ALSFRS_R records 
dim(alsfrs_wide) # 59,311 datapoints 
length(unique(alsfrs_wide$SubjectID)) # 6,514 patients
table(table(alsfrs_wide$SubjectID)) 

# Remove ALSFRS or ALSFRS-R records where not all items are available
alsfrs_wide -> temp
A = unique(temp[is.na(temp$R1_Dyspnea),]$SubjectID)
B = unique(temp[is.na(temp$R2_Orthopnea),]$SubjectID)
C = unique(temp[is.na(temp$R3_Respiratory_Insufficiency),]$SubjectID)
D = union(union(A,B),C) # any missing of R1,R2,R3

allSubject = unique(alsfrs_wide$SubjectID)
alsfrs_revised_subject = setdiff(allSubject, D) # 3406 patients with R1,R2,R3 full records
# 3108 patients with any missing in R1,R2,R3 

alsfrs_wide %>%
  filter(SubjectID %in% D) %>%
  select(-c(ALSFRS_R_Total, Q5a_Cutting_without_Gastrostomy, 
            Q5b_Cutting_with_Gastrostomy,
         R1_Dyspnea, R2_Orthopnea, R3_Respiratory_Insufficiency, 
         respiratory_R)) -> alsfrs_not_complete_revised
# Exclude data with any missing components 
alsfrs_not_complete_revised = 
  alsfrs_not_complete_revised[complete.cases(alsfrs_not_complete_revised),]

alsfrs_wide %>%
  filter(SubjectID %in% alsfrs_revised_subject) %>%
  select(-c(ALSFRS_Total, Q5a_Cutting_without_Gastrostomy, 
            Q5b_Cutting_with_Gastrostomy,
            Q10_Respiratory, respiratory)) -> alsfrs_revised
# Exclude data with any missing components 
alsfrs_revised = alsfrs_revised[complete.cases(alsfrs_revised),]

# Convert ALSFRS-R to ALSFRS: 
# R1_Dyspnea,R3_Respiratory_Insufficiency 데이터에서 Q10R 점수로 변환
alsfrs_revised -> temp 
temp$Q10R = ifelse(temp$R1_Dyspnea == 4, 4, NA)
temp$Q10R = ifelse(((temp$R1_Dyspnea >= 2) & (temp$R1_Dyspnea < 4)), 3, temp$Q10R)
temp$Q10R = ifelse(((temp$R1_Dyspnea >= 1) & (temp$R1_Dyspnea < 2)), 2, temp$Q10R)
temp$Q10R = ifelse(((temp$R1_Dyspnea < 1) &
                    (temp$R3_Respiratory_Insufficiency >=2)), 1, temp$Q10R)
temp$Q10R = ifelse(((temp$R1_Dyspnea < 1) & 
                    (temp$R3_Respiratory_Insufficiency <2)), 0, temp$Q10R)
temp %>% select(-c(R1_Dyspnea, R2_Orthopnea, R3_Respiratory_Insufficiency, 
                   respiratory_R, ALSFRS_R_Total)) -> alsfrs_revised

alsfrs_not_complete_revised-> temp
temp$Q10R = temp$Q10_Respiratory 
temp$Q10R = ifelse(((temp$Q10R > 3) & (temp$Q10R < 4)), 3, temp$Q10R)
temp$Q10R = ifelse(((temp$Q10R > 2) & (temp$Q10R < 3)), 2, temp$Q10R)
temp %>% select(-c(ALSFRS_Total, Q10_Respiratory, respiratory)) -> alsfrs_not_complete_revised

# Q10R에 따라 ALSFRS_Total 계산
alsfrs_not_complete_revised %>%
  mutate(ALSFRS_Total = Q1_Speech + Q2_Salivation + Q3_Swallowing +
                            Q4_Handwriting + Q5_Cutting +
                            Q6_Dressing_and_Hygiene + Q7_Turning_in_Bed +
                            Q8_Walking + Q9_Climbing_Stairs + Q10R) -> temp_original
alsfrs_revised %>%
  mutate(ALSFRS_Total = Q1_Speech + Q2_Salivation + Q3_Swallowing +
           Q4_Handwriting + Q5_Cutting +
           Q6_Dressing_and_Hygiene + Q7_Turning_in_Bed +
           Q8_Walking + Q9_Climbing_Stairs + Q10R) -> temp_revised
rbind(temp_original, temp_revised) -> alsfrsfull

# Exclude datapoints with feature_delta < 0 
alsfrsfull %>%
  filter(feature_delta >= 0) -> alsfrsfull
alsfrsfull = as.data.frame(alsfrsfull)
alsfrsfull$respiratory = alsfrsfull$Q10R
dim(alsfrsfull) # 59193 complete ALSFRS or ALSFRS-R records 
length(unique(alsfrsfull$SubjectID)) # 6507 patients

# ALS MITOS 추가
Movement <- (alsfrsfull$Q8_Walking <=1 | alsfrsfull$Q6_Dressing_and_Hygiene<=1)
Swallowing <- (alsfrsfull$Q3_Swallowing <= 1 )
Communicating <- (alsfrsfull$Q1_Speech<=1 & alsfrsfull$Q4_Handwriting <=1)
Breathing <- (alsfrsfull$Q10R <= 1)
ALSMITOS <- Movement + Swallowing + Communicating + Breathing
alsfrsfull <- mutate(alsfrsfull, Movement,Swallowing,Communicating,Breathing,ALSMITOS)

write.csv(alsfrsfull, "ALSFRS_MITOS_all.csv", quote = F, row.names = F)

