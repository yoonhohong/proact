library(tidyverse)

# Set working directory for scripts 
setwd("~/Dropbox/Github/proact/staging")

# Read ALSFRS_rev.csv file from data set folder 
alsfrs = read.csv("~/Dropbox/ALSmaster/PROACT_data/PROACT_preprocessed/ALSFRS_rev.csv")

# Collapse item scores from 5 levels to 3  
# For the first 11 items (Q1-Q11), the 5 original response levels were reduced to 3, collapsing level 0 and 1 into 0, and level 2 and 3 into 1, and level 4 into into 2. 
# For the item 12 ‘respiratory insufficiency’, the 5 levels were collapsed into the following three response options: 2 = no respiratory insufficiency (4 in original level); 1 = use of BiPAP (3, 2, 1 in original level); 0 = invasive mechanical ventilation (0 in original level) 
# Define function 

collapse1 = function(x){
  y = case_when(
    x %in% c(0,1) ~ 0,
    x %in% c(2,3) ~ 1,
    TRUE ~ 2)
  y
}
collapse2 = function(x){
  y = case_when(
    x == 0 ~ 0,
    x %in% c(1,2,3) ~ 1,
    TRUE ~ 2)
  y
}

# 4번째(Q1_Speech)부터 14번째(R2_Orthopnea) 열까지 collapse1 적용  
# 15번째(R3_Respiratory_Insufficiency) 열에 collapse2 적용 

df1 = sapply(alsfrs[,4:14], collapse1)
df2 = sapply(alsfrs[,15], collapse2)
df = data.frame(df1, R3_Respiratory_Insufficiency = df2)
temp = cbind(alsfrs[,1:3], df, alsfrs[,16:19])

# Calculate ALSFRS_R_Total, Functional domain (bulbar, motor, respiratory) score 
temp[,3] = apply(temp[,4:15], 1, sum)
temp[,16] = apply(temp[,4:6], 1, sum)
temp[,17] = apply(temp[,7:12], 1, sum)
temp[,18] = apply(temp[,13:15], 1, sum)

alsfrs_r_collapse = temp
write.csv(alsfrs_r_collapse, "~/Dropbox/ALSmaster/PROACT_data/PROACT_preprocessed/ALSFRS_rev_collapsed.csv")





