library(tidyverse)
library(broom)


# BMR stage dataset
bmr <- read.csv("stage_bmr.csv")
# dead subjects dataset
surv <- read.csv("survival.csv")
# King and MiToS stage dataset, subjectid의 이름은 bmr stage dataset과 같은 SubjectID로 변경 
king_mitos <- read.csv("stage_king_mitos.csv") %>% rename(SubjectID=subjectid)
# ALSFRS-R score dataset
alsfrs_r <- read.csv("alsfrs_rev.csv")



# exclude patients with only one record and merge the ALSFRS-R score to each subject at each visit time 
temp_bmr <- bmr %>% group_by(SubjectID) %>% 
  mutate(visit=n()) %>% 
  ungroup() %>% 
  filter(visit>=2) %>% 
  left_join(alsfrs_r,by=c("SubjectID","feature_delta"))
temp_king_mitos <- king_mitos %>% group_by(SubjectID) %>% 
  mutate(visit=n()) %>% 
  ungroup() %>% 
  filter(visit>=2) %>% 
  left_join(alsfrs_r,by=c("SubjectID","feature_delta"))



# ALSFRS-R trajectory according to BMR stage exclude stage 7 which has no ALSFRS-R score
temp_bmr %>% filter(bmr_stage<=6) %>% ggplot(aes(feature_delta,ALSFRS_R_Total,col=factor(bmr_stage),group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~bmr_stage,ncol=3,nrow=3)+
  scale_x_continuous(limits=c(0,max(temp_bmr$feature_delta)))+
  scale_color_discrete(name="ALSFRS-R slope per month")+
  labs(x="Visit time (months)")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank())



# ALSFRS-R trajectory according to King stage exclude stage 5 which has no ALSFRS-R score
temp_king_mitos %>% filter(king<=4) %>% ggplot(aes(feature_delta,ALSFRS_R_Total,col=factor(king),group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~king,ncol=3,nrow=2)+
  scale_x_continuous(limits=c(0,max(temp_king_mitos$feature_delta)))+
  scale_color_discrete(name="ALSFRS-R slope per month")+
  labs(x="Visit time (months)")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank())



# ALSFRS-R trajectory according to MiToS stage exclude stage 5 which has no ALSFRS-R score
temp_king_mitos %>% filter(mitos<=4) %>% ggplot(aes(feature_delta,ALSFRS_R_Total,col=factor(mitos),group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~mitos,ncol=3,nrow=2)+
  scale_x_continuous(limits=c(0,max(temp_king_mitos$feature_delta)))+
  scale_color_discrete(name="ALSFRS-R slope per month")+
  labs(x="Visit time (months)")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank())
