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
bmr <- read.csv("stage_bmr.csv") %>% rename(SubjectID=subjectid)
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
