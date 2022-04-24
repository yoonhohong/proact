# install package: msm 
# set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# read ALSFRS stage dataset 
stage = read.csv("ALSFRS_stage.csv")
stage = within(stage, {
  king = factor(king)
  mitos = factor(mitos)
})

stage = stage %>%
  group_by(SubjectID) %>% # we need data grouped by subject
  arrange(SubjectID, feature_delta) # arranged by time 

dim(stage)
stage = distinct(stage)
dim(stage)

stage = stage %>%
  group_by(SubjectID, feature_delta) %>%
  summarise(n = n(), king, mitos, ALSFRS_Total, 
            bulbar, motor, respiratory) %>%
  filter(n < 2) %>%
  select(-n)
dim(stage)

temp = stage %>%
  group_by(SubjectID) %>%
  summarise(n = n()) %>%
  filter(n > 1)
stage = stage %>%
  filter(SubjectID %in% temp$SubjectID)
dim(stage)

# Cumulative density, King's stage and MiToS stage 
stage %>%
  ggplot(aes(feature_delta, color = king)) + 
  stat_ecdf() + 
  scale_x_continuous(limits = quantile(stage$feature_delta, 
                                       prob = c(0,0.99)), 
                     name = "Time from enrollment (months)") + 
  scale_y_continuous(name = "Cumulative density")
  
stage %>%
  ggplot(aes(feature_delta, color = mitos)) + 
  stat_ecdf() + 
  scale_x_continuous(limits = quantile(stage$feature_delta, 
                                       prob = c(0,0.99)), 
                     name = "Time from enrollment (months)") + 
  scale_y_continuous(name = "Cumulative density")


library(msm)
library(dplyr)

# summary of multi-state data 
# as a frequency table of pairs of consecutive states   
# counts over all individuals, for each state r and s, 
# the number of times an individual had an observation of state r 
# followed by an observation of state s
statetable.msm(state = king, subject = SubjectID, data = stage) # King's stage
# 31 deaths from state 1, 732 death from state 4 
# deaths from stage 1, 2, 3? 
# resurrection? 
# recovery? 
statetable.msm(mitos, SubjectID, data = stage) # MiToS's stage 
# by default; stage begins at 1, which correponds to mitos stage 0  

# specification of the multi-state model 
# assumption...
# markov process
# how many states? 
# advance from consecutive states while alive 
# no recovery 
# no jumping through states, skipping in-between states  
# can not die from any state before stage 4 (King's stage) 
# misclassification possible

# first, model mitos stage as Markov process following the above assumption
# initial transition intensity matrix 
stage %>%
  mutate(mitos = mitos + 1) -> temp
Q = rbind( c(0, 0.2, 0, 0, 0, 0), 
           c(0, 0, 0.3, 0, 0, 0), 
           c(0, 0, 0, 0.2, 0, 0), 
           c(0, 0, 0, 0, 0.2, 0),
           c(0, 0, 0, 0, 0, 0.3), 
           c(0, 0, 0, 0, 0, 0) )
Q.crude = crudeinits.msm(mitos ~ feature_delta, 
                         SubjectID, data = temp, 
                         qmatrix = Q)
# alternatively, you can specify initial values by 
# calculating the crude estimates of q_rs 
# q_rs = n_rs/T_r 
# q_rs: transition intensity (or rate) from state r to s 
# n_rs: number of transitions from state r to s 
# T_r; a total duration spent in state r 

# misclassification, e-matrix 

mitos.msm = msm( mitos ~ feature_delta, subject = SubjectID, 
               data = temp, 
               qmatrix = Q.crude, 
               deathexact = 6)

# distribution of feature_delta interval

library(purrr)

# distribution of number of visits 
stage %>%
  group_by(SubjectID) %>%
  count() -> temp 
temp %>%
  ggplot(aes(n)) + 
  geom_histogram(col = "white") 
summary(temp$n)

# calculate sojourn time for each stage 
# caldulate delta interval for each individual 
calculate_delta = function(x){
  temp = c()
  for (i in 1:(length(x)-1)) {
    temp[i] = x[i+1] - x[i]
  }
  return(temp)
} 
stage %>%
  group_by(SubjectID) %>%
  group_map(~ calculate_delta(.x$feature_delta)) -> temp2






