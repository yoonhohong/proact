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

# ALSFRS total score and subscore divided into 3 domains according to BMR stage
alsfrs_bmr %>% group_by(bmr_stage) %>% summarise(alsfrs_mean=mean(ALSFRS_R_Total),alsfrs_sd=sd(ALSFRS_R_Total),
                                                 bulbar_mean=mean(bulbar),bulbar_sd=sd(bulbar),
                                                 motor_mean=mean(motor),motor_sd=sd(motor),
                                                 respiratory_mean=mean(respiratory),respiratory_sd=sd(respiratory))
p_alsfrs_bmr <- alsfrs_bmr %>% ggplot(aes(x=factor(bmr_stage),y=ALSFRS_R_Total))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="BMR stage",y="ALSFRS-R")


# ALSFRS total score and subscore divided into 3 domains according to King stage
alsfrs_king_mitos %>% group_by(king) %>% summarise(alsfrs_mean=mean(ALSFRS_R_Total),alsfrs_sd=sd(ALSFRS_R_Total),
                                                 bulbar_mean=mean(bulbar),bulbar_sd=sd(bulbar),
                                                 motor_mean=mean(motor),motor_sd=sd(motor),
                                                 respiratory_mean=mean(respiratory),respiratory_sd=sd(respiratory))
p_alsfrs_king <- alsfrs_king_mitos %>% ggplot(aes(x=factor(king),y=ALSFRS_R_Total))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="king's stage",y="ALSFRS-R")



# ALSFRS total score and subscore divided into 3 domains according to BMR stage
alsfrs_king_mitos %>% group_by(bmr_stage) %>% summarise(alsfrs_mean=mean(ALSFRS_R_Total),alsfrs_sd=sd(ALSFRS_R_Total),
                                                 bulbar_mean=mean(bulbar),bulbar_sd=sd(bulbar),
                                                 motor_mean=mean(motor),motor_sd=sd(motor),
                                                 respiratory_mean=mean(respiratory),respiratory_sd=sd(respiratory))
p_alsfrs_mitos <- alsfrs_king_mitos %>% ggplot(aes(x=factor(mitos),y=ALSFRS_R_Total))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="MiToS stage",y="ALSFRS-R")