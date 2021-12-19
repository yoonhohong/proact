library(readxl)
library(ggplot2)
library(survival)
library(survminer)
library(extrafont)
library(dplyr)
library(tidyr)
library(survival)
library(writexl)
library(lubridate)
library(gmodels)
library(moonBook)
# install.packages("nparcomp") #kruskal walis test에서의 사후검정
# install.packages("rmarkdown")
library(nparcomp)
library(tidyverse)
library(dlookr)
library(broom)
library(gridExtra)
library(naniar)
# install.packages("officer")
# install.packages("epiDisplay")
library(officer)
# install.packages("VIM")
# install.packages("tidyverse")
library(VIM)
# install.packages("dlookr")
library(dlookr)
# install.packages("data.table")
library(data.table)
# install.packages("RColorBrewer")
library(RColorBrewer)
options("scipen"=100)
options(max.print=1000000)





# BMR stage dataset
bmr <- read.csv("stage_bmr.csv")
# ALSFRS-R score dataset
alsfrs_r <- read.csv("alsfrs_rev.csv")




# merge ALSFRS-R score dataset and BMR stage dataset
alsfrs_bmr <- bmr %>% left_join(alsfrs_r,by=c("SubjectID","feature_delta"))



# characteristics of each stage
# distribution of ALSFRS-R total and domain scores at each stage
alsfrs_bmr %>% group_by(bmr_stage) %>% summarise(alsfrs_mean=mean(ALSFRS_R_Total),alsfrs_sd=sd(ALSFRS_R_Total),
                                                 bulbar_mean=mean(bulbar),bulbar_sd=sd(bulbar),
                                                 motor_mean=mean(motor),motor_sd=sd(motor),
                                                 respiratory_mean=mean(respiratory),respiratory_sd=sd(respiratory))
p_alsfrs <- alsfrs_bmr %>% ggplot(aes(x=factor(bmr_stage),y=ALSFRS_R_Total))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="BMR stage",y="ALSFRS-R")
p_bulbar <- alsfrs_bmr %>% ggplot(aes(x=factor(bmr_stage),y=bulbar))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="BMR stage",y="ALSFRS-R bulbar subscore")
p_motor <- alsfrs_bmr %>% ggplot(aes(x=factor(bmr_stage),y=motor))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="BMR stage",y="ALSFRS-R motor subscore")
p_respiratory <- alsfrs_bmr %>% ggplot(aes(x=factor(bmr_stage),y=respiratory))+
  geom_boxplot(width=0.8,outlier.size=3,outlier.shape=16,outlier.color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="BMR stage",y="ALSFRS-R respiratory subscore")
box_plot_bmr <- arrangeGrob(p_alsfrs,p_bulbar,p_motor,p_respiratory,nrow=2,ncol=2)
ggsave(box_plot_bmr,file="D:/홍윤호교수님 연구/BMR staging1/boxplot_ALSFRS-R_BMR.tiff",dpi = 300)



# at what stages are patients enrolled? 
enroll_stage_bmr <- alsfrs_bmr %>% filter(feature_delta==first(feature_delta)) %>% 
  ggplot(aes(bmr_stage))+
  geom_bar()+
  labs(x="BMR stage",y="number of subjects")+
  scale_x_continuous(breaks = seq(0,7))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggsave(enroll_stage_bmr,file="D:/홍윤호교수님 연구/BMR staging1/enroll_stage_bmr.tiff",dpi = 300)



# estimate the slope of ALSFRS-R total score within each stage (using linear regression), and then compare the slope of ALSFRS-R total score across stages 
alsfrs_slope_bmr <- alsfrs_bmr %>% ggplot(aes(x=bmr_stage,y=ALSFRS_R_Total))+
  stat_summary(fun.data = mean_cl_normal)+
  geom_smooth(method='lm',formula = y~x)+
  labs(x="BMR stage",y="ALSFRS-R")+
  scale_x_continuous(breaks = seq(0,7))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggsave(alsfrs_slope_bmr,file="D:/홍윤호교수님 연구/BMR staging1/alsfrs_slope_bmr.tiff",dpi = 300)



# Changes of stages with disease progression 
alsfrs_bmr %>% group_by(SubjectID) %>% mutate(bmr_stage_enroll=first(bmr_stage)) %>% 
  ungroup() %>% group_by(bmr_stage_enroll) %>% 
  ggplot(aes(x=feature_delta,y=bmr_stage,col=factor(bmr_stage_enroll)))+
  geom_line(alpha=0.2)+
  labs(x="Elapsed time from enrollment(month)",y="BMR stage")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())



# distribution of ALS stage last recorded in 12 month period by baseline stage, exclude patients with fu period of less than 9 months 
# feature_delta_12mo: f/u data가 9~15개월의 데이터가 있는 대상자만 filtering
feature_delta_12mo <- alsfrs_bmr %>% filter(between(feature_delta,9,15))
# feature_delta_12mo_1: f/u data 9~15개월의 데이터 중 12개월에 가장 근접한 데이터만 추출 
feature_delta_12mo_1 <- feature_delta_12mo %>% group_by(SubjectID) %>% 
  mutate(temp_feat=abs(feature_delta-12)) %>% 
  filter(temp_feat==min(temp_feat)) %>% 
  dplyr::select(-temp_feat)
#  ALSFRS데이터셋 중 feature_delta_12mo_1에 있는 ID만 추출
feature_delta_12mo_baseline <- alsfrs_bmr %>% filter(SubjectID%in%feature_delta_12mo_1$SubjectID) %>% 
  group_by(SubjectID) %>% 
  filter(feature_delta==first(feature_delta))
# feature delta 12개월근사값을 가진 subject들의 baseline과 12개월쯤의 f/u stage dataset merge
alsfrs_12mo <- feature_delta_12mo_baseline %>% bind_rows(feature_delta_12mo_1) %>% 
  arrange(SubjectID,feature_delta) %>% 
  group_by(SubjectID) %>% 
  mutate(baseline_bmr=first(bmr_stage),
         fu_bmr=last(bmr_stage))
# baseline stage에 따라서 12개월쯤(9~15개월 사이 f/u데이터 중 12개월에 가장 근접한 f/u 시점)의 stage변화
alsfrs_12mo %>% ggplot(aes(factor(baseline_bmr)))+
  geom_bar(aes(fill=factor(fu_bmr),position="dodge",stat="identity"))+
  theme_bw()+
  labs(x="Baseline BMR stage",y="Number of subject")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  


# standardised time from symptom onset to each stage for only deceased patients 
# alsfrs_death: deceased patient dataset
surv <- read.csv("survival.csv")
clinical_info <- read.csv("PROACT_preprocessed.csv")
# clinical info정보 import, onset으로부터 경과시간 계산 
alsfrs_death <- alsfrs_bmr %>% filter(SubjectID %in% surv$SubjectID) %>% merge(clinical_info, all.x = T, by="SubjectID")
alsfrs_death <- alsfrs_death %>% mutate(time_from_onset=round(abs(onset_delta)/30,2)+feature_delta,
                                        time_from_diag=round(abs(diag_delta)/30,2)+feature_delta)
# standardised time 계산 
alsfrs_death <- alsfrs_death %>% group_by(SubjectID) %>% 
  mutate(stan_time=time_from_onset/max(time_from_onset))
# standardised median time plot, subject별로 BMR stage내에서 stage변하기 직전의 time from onset으로 standardized time계산
# BMR stage별 standardized median time plot 
alsfrs_death %>% arrange(SubjectID,feature_delta) %>% 
  group_by(SubjectID,bmr_stage) %>% 
  filter(feature_delta==max(feature_delta)) %>% 
  ggplot(aes(factor(bmr_stage),stan_time,fill=factor(bmr_stage)))+
  geom_boxplot()+
  theme(panel.grid = element_blank())+
  coord_flip()+
  labs(x="BMR stage",y="Standardized median time",fill="BMR stage")



# stage transition probabilities (based on Markov model) waterfall plot to display how the stage progress with time stay at each stage 







save.image("BMR staging analysis.RData")
load("BMR staging analysis.RData")
