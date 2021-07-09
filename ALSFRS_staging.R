# Read-in ALSFRS_original data
alsfrs <- read.csv("ALSFRS_original.csv")

dim(alsfrs) # 59,260 observations 
length(unique(alsfrs$SubjectID)) # 6,510 patients

# King's staging: 
# Q5b_Cutting_with_Gastrostomy가 NA값이 아니면 4A, 
# Q10_Respiratory값이 0이면 4B, 
# 4A나 4B가 아닌 것들 중,  
# mouth<12, hands<8, leg<4중 (+)인 갯수-> stage1~3  by JNNP

alsfrs$bulbar <- as.integer(ifelse(alsfrs$mouth<12,1,0))
alsfrs$upper <- as.integer(ifelse(alsfrs$Q4_Handwriting<4|
                    alsfrs$Q5a_Cutting_without_Gastrostomy<4,1,0))
alsfrs$lower <- as.integer(ifelse(alsfrs$Q8_Walking<4,1,0))
alsfrs$b_u_l <- alsfrs$bulbar+alsfrs$upper+alsfrs$lower 

#bulbar, 상지, 하지 중 involve된 부분의 갯수 변수 
#alsfrs$King_J <- ifelse(alsfrs$Q5b_Cutting_with_Gastrostomy!=NA|alsfrs$Q10_Respiratory==0, 4, NA) 
alsfrs$King_J <- ifelse((!is.na(alsfrs$Q5b_Cutting_with_Gastrostomy))|(alsfrs$Q10_Respiratory==1), 4, NA) 

# gastrostomy를 받은 환자나 NIV를 사용하는 환자는 
# stage4로 따로 분류하고 나머지는 NA로 
alsfrs$King <- ifelse(is.na(alsfrs$King_J), alsfrs$b_u_l, alsfrs$King_J) 
# stage 4가 아닌 환자 중 bulbar, 상지, 하지 중 involve된 부분으로 
# stage 1~3중 부여 

table(alsfrs$King) 
# stage = 0 in 481 patients ?? 
# 481/59,260 observations (0.8%) 

# library(dplyr)
# temp = alsfrs %>%
#   filter(King == 0)
#   
# summary(temp)
# Q6_Dressing_and_Hygiene
# Q7_Turning_in_Bed
# Q9_Climbing_Stairs 
# Q10_Respiratory

alsfrs$King = ifelse(alsfrs$King == 0, 1, alsfrs$King)

# MiTos staging

# speech(var[4]), 
# swallowing(var[6]), 
# handwriting(var[7]), dressing/hygiene(var[11]), 
# walking(var[13]), dyspnoea(?), respiratory insufficiency(var[15]) by JNNP
# alsfrs$thoracic <- as.integer(ifelse(alsfrs$trunk<8,1,0))
# alsfrs$MiTos <- alsfrs$b_u_t_l
# write.csv(alsfrs,"ALSFRS_stage.csv")

alsfrs$swallowing = ifelse(alsfrs$Q3_Swallowing <= 1, 1, 0)
alsfrs$breathing = ifelse(alsfrs$Q10_Respiratory <= 2, 1, 0)
alsfrs$communicating = ifelse(alsfrs$Q1_Speech <= 1 & alsfrs$Q4_Handwriting <= 1, 
                              1, 0)
alsfrs$movement = ifelse(alsfrs$Q6_Dressing_and_Hygiene <= 1|alsfrs$Q8_Walking <=1, 
                         1, 0)
alsfrs = alsfrs %>%
  mutate(Mitos = swallowing + breathing + communicating + movement)


table(alsfrs$Mitos, alsfrs$King)

stage = alsfrs %>%
  select(SubjectID, feature_delta, ALSFRS_Total, King, Mitos)

stage = stage[complete.cases(stage),]
dim(stage) # 59226 observations 
length(unique(stage$SubjectID)) # 6510 patients 

# ALSFRS_Total and stage (King, Mitos) at enrollment 
# Merge with survival data (death = stage 5 in Mitos and King's staging)

surv = read.csv('survival.csv')
dim(surv) # 9,080 patients 
death = surv %>%
  filter(status == 1)
dim(death) # 3,075 patients who died 

death = within(death, {
  feature_delta <- time_event
  ALSFRS_Total <- NA
  King <- 5
  Mitos <- 5
  rm(time_event, status)
})

death = death %>%
  select(SubjectID, feature_delta, ALSFRS_Total, King, Mitos)

stage_final = rbind(stage, death)
dim(stage_final) # 62,301 observations; 59,226 ALSFRS + 3,075 death
length(unique(stage_final$SubjectID)) # 7,808 patients; 6,510 patients with at least one ALSFRS 

stage_final = within(stage_final, {
  King = factor(King)
  Mitos = factor(Mitos)})


write.csv(stage_final, "ALSFRS_stage.csv", quote = F, row.names = F)

###############

library(ggplot2)
ggplot(stage_final, aes(King, feature_delta)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(limits = quantile(stage$feature_delta, c(0.1, 0.9))) +
  labs(title = "King stage", y = "Time from enrollment (Days)")

ggplot(stage_final, aes(Mitos, feature_delta)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(limits = quantile(stage$feature_delta, c(0.1, 0.9)))+
  labs(title = "MITOS stage", y = "Time from enrollment (Days)")

ggplot(stage_final, aes(King, ALSFRS_Total)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(limits = quantile(stage$ALSFRS_Total, c(0.1, 0.9))) +
  labs(title = "King's stage")

ggplot(stage_final, aes(Mitos, ALSFRS_Total)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(limits = quantile(stage$ALSFRS_Total, c(0.1, 0.9))) +
  labs(title = "MITOS stage")

temp = stage_final %>%
  group_by(SubjectID) %>%
  arrange(feature_delta) %>%
  mutate(visit = rank(feature_delta))

first = temp %>% filter(visit == 1 & feature_delta < 90)
summary(first$feature_delta)

dim(first) # 6505 patients 

interval = max(first$ALSFRS_Total) - min(first$ALSFRS_Total)

ggplot(first, aes(ALSFRS_Total)) + 
  geom_histogram(bins = interval, col = 'white') + 
  labs(title = "Distriubtion of ALSFRS Total score at enrollment")
# at enrollment; the first visit within 3 months (mostly at feature_delta == 0)

t3 = data.frame(round(prop.table(table(first$King))*100))
t4 = data.frame(round(prop.table(table(first$Mitos))*100))

ggplot(t3, aes(Var1, Freq)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Distribution of King's stage at enrollment", 
       x = "King's stage", 
       y = "Percentage (%)")

ggplot(t4, aes(Var1, Freq)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Distribution of Mitos stage at enrollment", 
       x = "Mitos stage", 
       y = "Percentage (%)")


last = temp %>% filter(visit == last(visit) & feature_delta >= 90)

dim(last) #  7,364 patients 

temp_last = temp %>%
  filter(!is.na(ALSFRS_Total))

interval = max(temp_last$ALSFRS_Total) - min(temp_last$ALSFRS_Total)

ggplot(temp_last, aes(ALSFRS_Total)) + 
  geom_histogram(bins = interval, col = 'white') + 
  labs(title = "Distriubtion of ALSFRS Total score at enrollment")
# at enrollment; the last visit within 3 months (mostly at feature_delta == 0)

t3 = data.frame(round(prop.table(table(last$King))*100))
t4 = data.frame(round(prop.table(table(last$Mitos))*100))

dur = ((max(last$feature_delta) - min(last$feature_delta))/365)*12
ggplot(last, aes(feature_delta)) + 
  geom_histogram(bins = dur, col="white") 

ggplot(t3, aes(Var1, Freq)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Distribution of King's stage at close", 
       x = "King's stage", 
       y = "Percentage (%)")

ggplot(t4, aes(Var1, Freq)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Distribution of Mitos stage at close", 
       x = "Mitos stage", 
       y = "Percentage (%)")


