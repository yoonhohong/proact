library(tidyverse)
library(gridExtra)


# BMR stage dataset
bmr <- read.csv("stage_bmr.csv")
# King and MiToS stage dataset, subjectid의 이름은 bmr stage dataset과 같은 SubjectID로 변경 
king_mitos <- read.csv("stage_king_mitos.csv") %>% rename(SubjectID=subjectid)


# exclude patients with only one record and merge the ALSFRS-R score to each subject at each visit time 
temp_bmr <- bmr %>% group_by(SubjectID) %>% 
  mutate(visit=n(),first_bmr=first(bmr_stage)) %>% 
  ungroup() %>% 
  filter(visit>=2)
temp_king_mitos <- king_mitos %>% group_by(SubjectID) %>% 
  mutate(visit=n(),first_king=first(king),first_mitos=first(mitos)) %>% 
  ungroup() %>% 
  filter(visit>=2) 



# changes of BMR stages with disease progression 
p_bmr <- temp_bmr %>% filter(bmr_stage<=6) %>% ggplot(aes(feature_delta,bmr_stage,col=factor(first_bmr),group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~factor(first_bmr),ncol=2,nrow=3)+
  labs(x="elapsed time from enrollment",y="BMR stage")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(0,8))+
  geom_smooth(method=lm) 



# changes of King's stages with disease progression 
p_king <- temp_king_mitos %>% filter(king<=5) %>% ggplot(aes(feature_delta,king,col=factor(first_king),group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~factor(first_king),ncol=2,nrow=2)+
  labs(x="elapsed time from enrollment",y="King's stage")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(0,5))+
  geom_smooth(method=lm) 



# changes of MiToS stages with disease progression 
p_mitos <- temp_king_mitos %>% filter(mitos<=5) %>% ggplot(aes(feature_delta,mitos,col=factor(first_mitos),group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~factor(first_mitos),ncol=2,nrow=2)+
  labs(x="elapsed time from enrollment",y="MiToS stage")+
  theme_bw()+
  theme(legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_y_continuous(limits=c(0,5))+
  geom_smooth(method=lm) 

grid.arrange(p_bmr,p_king,p_mitos,ncol=1)
