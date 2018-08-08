# Baseline descriptive statistics 

# Read proact data, preprocessed and cleaned 
proact = read.csv("PROACT_preprocessed_cleaned.csv")

# Read data in which left censored subjects are excluded... 
sub = read.csv("ALSFRS_excl_leftcensored.csv")
subj_sub = unique(sub$SubjectID)

# Summary statistics: all subjects 
# 6507 patients 
summary(proact)








