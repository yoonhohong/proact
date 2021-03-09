# predict the slope of ALSFRS total scores 

# features  
## age, onset region, onset_delta, ALSFRS total score, FVC  

# algorithm 
## lasso linear regression, random forest 

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# Import ALSFRS_slope dataset; target variable  
alsfrs_slope = read.csv("ALSFRS_slope.csv")
dim(alsfrs_slope) # 3096 patients 

# Import ALSFRS_original, ALSFRS_revised datasets 
alsfrs_orig = read.csv("ALSFRS_original.csv") # 6514 patients 

## all patients who are given ALSFRS_slope value for target variable (n=3096) 
## had a dataset of ALSFRS_original total scores. 
## only about a half of the patients also had a dataset of 
## ALSFRS_revised total scores. 
## therefore, we are going to use ALSFRS original dataset 
# for predicting features. 

# alsfrs_rev = read.csv("ALSFRS_revised.csv")
# length(unique(alsfrs_orig$SubjectID)) # 6514 patients 
# length(unique(alsfrs_rev$SubjectID)) # 3412 patients 
# alsfrs_both_subj = intersect(unique(alsfrs_orig$SubjectID), 
#                              unique(alsfrs_rev$SubjectID))
# length(alsfrs_both_subj) # 3412 patients 
# all(alsfrs_both_subj == unique(alsfrs_rev$SubjectID))
# length(intersect(alsfrs_slope$SubjectID, alsfrs_orig$SubjectID)) # 3096 patients
# length(intersect(alsfrs_slope$SubjectID, alsfrs_rev$SubjectID)) # 1594 patients









