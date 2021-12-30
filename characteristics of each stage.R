library(tidyverse)


# BMR stage dataset
bmr <- read.csv("stage_bmr.csv")
# dead subjects dataset
surv <- read.csv("survival.csv")
# King and MiToS stage dataset, subjectid의 이름은 bmr stage dataset과 같은 SubjectID로 변경 
king_mitos <- read.csv("stage_king_mitos.csv") %>% rename(SubjectID=subjectid)
# ALSFRS-R score dataset
alsfrs_r <- read.csv("alsfrs_rev.csv")




# merge ALSFRS-R score dataset and BMR stage dataset
alsfrs_bmr <- bmr %>% left_join(alsfrs_r,by=c("SubjectID","feature_delta"))
# merge ALSFRS-R score dataset and King and MiToS stage dataset
alsfrs_king_mitos <- king_mitos %>% left_join(alsfrs_r,by=c("SubjectID","feature_delta"))

# BMR stage 7인 subject들의 ALSFRS-R score확인 
alsfrs_bmr %>% filter(bmr_stage==7) %>% group_by(ALSFRS_R_Total) %>% tally()
# BMR stage 7인 subject의 ALSFRS-R 9,12,17,34인 경우 1명씩 제외, NA값은 남겨놓음 
temp <- alsfrs_bmr %>% filter(bmr_stage==7&!is.na(ALSFRS_R_Total))
alsfrs_bmr1 <- alsfrs_bmr %>% anti_join(temp)
rm(temp)
# ALSFRS total score and subscore divided into 3 domains according to BMR stage
alsfrs_bmr1 %>% group_by(bmr_stage) %>% summarise(alsfrs_mean=mean(ALSFRS_R_Total),alsfrs_sd=sd(ALSFRS_R_Total),
                                                 bulbar_mean=mean(bulbar),bulbar_sd=sd(bulbar),
                                                 motor_mean=mean(motor),motor_sd=sd(motor),
                                                 respiratory_mean=mean(respiratory),respiratory_sd=sd(respiratory))
# BMR stage-ALSFRS-R plot without BMR stage 7
p_alsfrs_bmr <- alsfrs_bmr1 %>% ggplot(aes(x=factor(bmr_stage),y=ALSFRS_R_Total))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(xlim = c(0,7))+
  labs(x="BMR stage",y="ALSFRS-R")


# King's stage 5인 subject들의 ALSFRS-R score확인 
alsfrs_king_mitos %>% filter(king==5) %>% group_by(ALSFRS_R_Total) %>% tally()
# King's stage 5인 subject의 ALSFRS-R 9,12,17,34인 경우 1명씩 제외, NA값은 남겨놓음 
temp <- alsfrs_king_mitos %>% filter(king==5&!is.na(ALSFRS_R_Total))
alsfrs_king_mitos1 <- alsfrs_king_mitos %>% anti_join(temp)
rm(temp)
# ALSFRS total score and subscore divided into 3 domains according to King stage
alsfrs_king_mitos %>% group_by(king) %>% summarise(alsfrs_mean=mean(ALSFRS_R_Total),alsfrs_sd=sd(ALSFRS_R_Total),
                                                 bulbar_mean=mean(bulbar),bulbar_sd=sd(bulbar),
                                                 motor_mean=mean(motor),motor_sd=sd(motor),
                                                 respiratory_mean=mean(respiratory),respiratory_sd=sd(respiratory))
# King's stage-ALSFRS-R plot without King's stage 5
p_alsfrs_king <- alsfrs_king_mitos1 %>% ggplot(aes(x=factor(king),y=ALSFRS_R_Total))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(xlim = c(1,4))+
  labs(x="King's stage",y="ALSFRS-R")



# MiToS stage 5인 subject들의 ALSFRS-R score확인 
alsfrs_king_mitos %>% filter(mitos==5) %>% group_by(ALSFRS_R_Total) %>% tally()
# MiToS stage 5인 subject의 ALSFRS-R 9,12,17,34인 경우 1명씩 제외, NA값은 남겨놓음 
temp <- alsfrs_king_mitos %>% filter(king==5&!is.na(ALSFRS_R_Total))
alsfrs_king_mitos1 <- alsfrs_king_mitos %>% anti_join(temp)
rm(temp)
# ALSFRS total score and subscore divided into 3 domains according to MiToS stage
alsfrs_king_mitos1 %>% group_by(mitos) %>% summarise(alsfrs_mean=mean(ALSFRS_R_Total),alsfrs_sd=sd(ALSFRS_R_Total),
                                                 bulbar_mean=mean(bulbar),bulbar_sd=sd(bulbar),
                                                 motor_mean=mean(motor),motor_sd=sd(motor),
                                                 respiratory_mean=mean(respiratory),respiratory_sd=sd(respiratory))
# MiToS stage-ALSFRS-R plot without MiToS stage 5
p_alsfrs_king <- alsfrs_king_mitos1 %>% ggplot(aes(x=factor(mitos),y=ALSFRS_R_Total))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(xlim = c(1,5))+
  labs(x="MiToS stage",y="ALSFRS-R")
