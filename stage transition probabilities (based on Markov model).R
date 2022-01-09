library(tidyverse)
library(gridExtra)
# install.packages("caret")
library(caret)
library(broom)
install.packages("reshape2")
library(reshape)

# BMR stage dataset
bmr <- read.csv("stage_bmr.csv")
# King and MiToS stage dataset, subjectid의 이름은 bmr stage dataset과 같은 SubjectID로 변경 
king_mitos <- read.csv("stage_king_mitos.csv") %>% rename(SubjectID=subjectid)



# 이전시점과 다음 방문 시점의 BMR stage를 비교하여 transition probability table 만듦 
bmr_tran <- bmr %>% group_by(SubjectID) %>% 
  mutate(bmr_stage_next=lead(bmr_stage)) %>% 
  filter(!is.na(bmr_stage_next))
bmr_tran_mtrx_prop <- prop.table(table(bmr_tran$bmr_stage,bmr_tran$bmr_stage_next),2)

king_tran <- king_mitos %>% group_by(SubjectID) %>% 
  mutate(king_stage_next=lead(king)) %>% 
  filter(!is.na(king_stage_next))
king_tran_mtrx_prop <- prop.table(table(king_tran$king,king_tran$king_stage_next),2)

mitos_tran <- king_mitos %>% group_by(SubjectID) %>% 
  mutate(mitos_stage_next=lead(mitos)) %>% 
  filter(!is.na(mitos_stage_next))
mitos_tran_mtrx_prop <- prop.table(table(mitos_tran$mitos,mitos_tran$mitos_stage_next),2)



# transition probability transformation: matrix to dataframe form 
bmr_tran_mtrx_prop1 <- melt(bmr_tran_mtrx_prop)
king_tran_mtrx_prop1 <- melt(king_tran_mtrx_prop)
mitos_tran_mtrx_prop1 <- melt(mitos_tran_mtrx_prop)



# transition probability plot with confusion matrix form
p_bmr <- bmr_tran_mtrx_prop1 %>% ggplot(aes(x=Var1,y=Var2,fill=value))+
  geom_raster()+
  geom_text(aes(label=round(value,2)))+ 
  labs(x="BMR stage",y="BMR stage of next visit",fill="Transition probability")+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,7,by=1))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p_king <- king_tran_mtrx_prop1 %>% ggplot(aes(x=Var1,y=Var2,fill=value))+
  geom_raster()+
  geom_text(aes(label=round(value,2)))+ 
  labs(x="King's stage",y="King's stage of next visit",fill="Transition probability")+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks=seq(0,5,by=1))+
  scale_y_continuous(breaks=seq(0,5,by=1))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p_mitos <- mitos_tran_mtrx_prop1 %>% ggplot(aes(x=Var1,y=Var2,fill=value))+
  geom_raster()+
  geom_text(aes(label=round(value,2)))+ 
  labs(x="MiToS stage",y="MiToS stage of next visit",fill="Transition probability")+
  scale_fill_viridis_c()+
  scale_x_continuous(breaks=seq(0,5,by=1))+
  scale_y_continuous(breaks=seq(0,5,by=1))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

grid.arrange(p_bmr,p_king,p_mitos,ncol=1)
       