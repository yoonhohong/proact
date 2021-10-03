# to investigate the relationship 
# between ALSFRS revised respiration item scores and 
# FVC and SVC measures 
# over the first 3 months after enrollment 

# alsfrs revised ov the first 3 mo
# fvc
# svc 
# target alsfrs slope from 3 to 12 mo 

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# import alsfrs revised item scores over the first 3 mo
# meta-features; mean and slope 
alsfrs = read.csv("alsfrs_revised_3mo_meta_slope.csv")  
dim(alsfrs) # 3273 patients 

# select respiration item scores 
alsfrs = alsfrs %>%
  select(SubjectID, 
         R1_mean, R2_mean, 
         R3_mean, 
         alsfrs_slope)

# import fvc meta-features over the first 3 mo 
fvc = read.csv("fvc_3mo_meta_slope.csv")

quantile(fvc$fvc_slope, na.rm = T)
dim(fvc) # 7218 patients 

# select mean values 
fvc = fvc %>%
  select(SubjectID, fvc_mean)

# fvc mean values in cases whose respiration item scores are all 4
alsfrs_sub = alsfrs %>%
  filter(R1_mean == 4, 
         R2_mean == 4, 
         R3_mean == 4)

dim(alsfrs_sub) # 1698 patients 

fvc_sub = fvc %>%
  filter(SubjectID %in% alsfrs_sub$SubjectID)

dim(fvc_sub) # 1225 patients 

fvc_sub %>%
  ggplot(aes(fvc_mean)) + 
  geom_histogram() 

quantile(temp$fvc_mean)

# alsfrs revised respiration item scores in cases 
# with fvc_mean > 60
fvc_sub2 = fvc %>%
  filter(fvc_mean > 60)

alsfrs_sub2 = alsfrs %>%
  filter(SubjectID %in% fvc_sub2$SubjectID) %>%
  select(SubjectID, 
         R1_mean, 
         R2_mean, 
         R3_mean)

dim(alsfrs_sub2) # 2209 patients 

summary(alsfrs_sub2)

library(tidyr)
temp = gather(alsfrs_sub2, key = "var", value = "value", 
              R1_mean:R3_mean)

temp %>% 
  ggplot(aes(var, value)) + 
  geom_boxplot()



