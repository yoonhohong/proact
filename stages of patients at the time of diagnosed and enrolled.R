library(tidyverse)

# BMR stage dataset
bmr <- read.csv("stage_bmr.csv")
# dead subjects dataset
surv <- read.csv("survival.csv")
# King and MiToS stage dataset, subjectid의 이름은 bmr stage dataset과 같은 SubjectID로 변경 
king_mitos <- read.csv("stage_king_mitos.csv") %>% rename(SubjectID=subjectid)

# 첫 방문시의 BMR stage 확인
# 0: 189, 1:10546, 2:9814, 3:6127, 4:1723, 5:344, 6:2
bmr %>% group_by(SubjectID) %>% 
  mutate(first_bmr=first(bmr_stage)) %>%
  ungroup() %>% 
  group_by(first_bmr) %>% 
  tally()

bmr %>% group_by(SubjectID) %>% 
  mutate(first_bmr=first(bmr_stage)) %>%
  ungroup() %>% 
  ggplot(aes(x=first_bmr))+
  geom_bar()+
  coord_cartesian(xlim=c(-1,6))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x="BMR stage at first visit",y="Number of subjects")


# 첫방문시의 King's stage확인
# 1:6878, 2:8228, 3:6700, 4:6939
king_mitos %>% group_by(SubjectID) %>% 
  mutate(first_king=first(king)) %>% 
  ungroup() %>% 
  group_by(first_king) %>% 
  tally()

king_mitos %>% group_by(SubjectID) %>% 
  mutate(first_king=first(king)) %>%
  ungroup() %>% 
  ggplot(aes(x=first_king))+
  geom_bar()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x="King's stage at first visit",y="Number of subjects")


# 첫방문시의 MiToS stage확인
# 0:23354, 1:4980, 2:380, 3:31
king_mitos %>% group_by(SubjectID) %>% 
  mutate(first_mitos=first(mitos)) %>% 
  ungroup() %>% 
  group_by(first_mitos) %>% 
  tally()

king_mitos %>% group_by(SubjectID) %>% 
  mutate(first_mitos=first(mitos)) %>%
  ungroup() %>% 
  ggplot(aes(x=first_mitos))+
  geom_bar()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x="MiToS stage at first visit",y="Number of subjects")
