# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# Load packages
library(dplyr)
library(tidyr)
library(survival)

# Read all datasets
data.allforms_training<-read.delim("all_forms_PROACT_training.txt",sep="|", header=T)
data.allforms_training2<-read.delim("all_forms_PROACT_training2.txt",sep="|", header=T)
data.allforms_leaderboard<-read.delim("all_forms_PROACT_leaderboard_full.txt",sep="|", header=T)
data.allforms_validation<-read.delim("all_forms_PROACT_validation_full.txt",sep="|", header=T)
data.allforms <- rbind(data.allforms_training,data.allforms_training2,data.allforms_leaderboard,data.allforms_validation)
length(unique(data.allforms$SubjectID)) # 10723 patients

# ALSFRS_slope datasets 
slope_training = read.delim("ALSFRS_slope_PROACT_training.txt", sep = "|", header = T)
slope_training2 = read.delim("ALSFRS_slope_PROACT_training2.txt", sep = "|", header = T)
slope_validation = read.delim("ALSFRS_slope_PROACT_validation.txt", sep = "|", header = T)
slope_leaderboard = read.delim("ALSFRS_slope_PROACT_leaderboard.txt", sep = "|", header = T)
slope_all = rbind(slope_training, slope_training2, slope_leaderboard, slope_validation)
dim(slope_all)
length(unique(slope_all$SubjectID)) # 3096 patients 
write.csv(slope_all, "ALSFRS_slope.csv", row.names = F, quote = F)

# Demographic
data.allforms %>% filter(form_name == "Demographic") %>% 
  select(SubjectID, feature_name, feature_value) -> temp
demographic = spread(temp, feature_name, feature_value) # 8653 patients 
length(unique(demographic$SubjectID))

demographic -> temp
temp$Age = round(as.numeric(temp$Age))
temp$Gender = factor(temp$Gender)
temp$Race = factor(temp$Race)
write.csv(temp, 'demographic.csv', row.names = F, quote = F)

# ALSFRS
data.all = data.allforms
alsfrs <- data.all %>%
  filter(form_name=="ALSFRS", )
alsfrs = alsfrs %>%
  select(-c(form_name,feature_unit))
alsfrs$feature_value <- as.numeric(as.character(alsfrs$feature_value))
alsfrs$feature_delta <- as.numeric(as.character(alsfrs$feature_delta))

# alsfrs records with feature_delta < 0 
range(alsfrs$feature_delta)
dim(temp[alsfrs$feature_delta <0,])[1] # 507 records 
length(unique(alsfrs[alsfrs$feature_delta <0,]$SubjectID)) # 27 patients 

# Exclude duplicates
# Exclude ALSFRS records with the same feature_delta 
# but different feature_value
dim(alsfrs)
temp = alsfrs
temp = unique(temp) 
dim(temp)
temp$id_delta = paste(temp$SubjectID, 
                      temp$feature_delta, sep = "_")
temp2 = temp %>%
  select(-feature_value)
temp3 = temp2[duplicated(temp2),]
duplicated_id_delta = unique(temp3$id_delta)
temp4 = temp %>%
  filter(!(id_delta %in% duplicated_id_delta)) %>%
  select(-id_delta)
temp4$feature_name = factor(temp4$feature_name)

alsfrs_wide <- spread(temp4,feature_name,feature_value)
dim(alsfrs_wide) #59,296 records 

# ALSFRS original vs. revised records 
orig = alsfrs_wide[!is.na(alsfrs_wide$ALSFRS_Total),]$SubjectID
orig_id = unique(orig)
rev = alsfrs_wide[!is.na(alsfrs_wide$ALSFRS_R_Total),]$SubjectID
rev_id = unique(rev)
length(intersect(orig_id, rev_id)) # 3,412 patients 
temp = alsfrs_wide %>%
  filter(!is.na(ALSFRS_Total)) %>%
  filter(!is.na(ALSFRS_R_Total))
dim(temp) # 30,165 records with both ALSFRS original and revised total scores 

# ALSFRS original 
alsfrs_original_wide = alsfrs_wide %>%
  filter(!is.na(ALSFRS_Total)) %>%
  select(-c(ALSFRS_R_Total, 
            R1_Dyspnea, R2_Orthopnea, 
            R3_Respiratory_Insufficiency, 
            respiratory_R))
dim(alsfrs_original_wide) # 59,287 records 
length(unique(alsfrs_original_wide$SubjectID)) # 6,514 patients 
table(table(alsfrs_original_wide$SubjectID))

temp = alsfrs_original_wide
summary(temp) # NA in Q10_Respiratory: 23063 records 
temp$Q10_Respiratory = ifelse(is.na(temp$Q10_Respiratory), 
                              temp$respiratory, 
                              temp$Q10_Respiratory) 
alsfrs_original_wide = temp
alsfrs_original_wide = 
  alsfrs_original_wide[,c(1,2,3,7,9:18,8,6,4,5,20,19)]

# ALSFRS revised 
alsfrs_revised_wide = alsfrs_wide %>%
  filter(!is.na(ALSFRS_R_Total)) %>%
  select(-c(ALSFRS_Total, 
            Q10_Respiratory, respiratory))
dim(alsfrs_revised_wide) # 30,167 records 
length(unique(alsfrs_revised_wide$SubjectID)) # 3,412 patients 
table(table(alsfrs_revised_wide$SubjectID))
alsfrs_revised_wide = 
  alsfrs_revised_wide[,c(1,2,3,7:20,6,4,5,22,21)]

write.csv(alsfrs_original_wide, "ALSFRS_original.csv",
          row.names = F, quote = F)
write.csv(alsfrs_revised_wide, "ALSFRS_revised.csv",
          row.names = F, quote = F)



# ALSFRS original vs. revised total scores
# meta-features during the first 3 months
alsfrs_total_3mo = alsfrs_original_wide %>%
  filter(feature_delta >=0 & feature_delta < 92) %>%
  select(SubjectID, ALSFRS_Total, feature_delta)

alsfrs_total_3mo_meta = alsfrs_total_3mo %>%
  group_by(SubjectID) %>% 
  summarise(mean_alsfrs_total = mean(ALSFRS_Total), 
            min_alsfrs_total = min(ALSFRS_Total), 
            max_alsfrs_total = max(ALSFRS_Total), 
            n=n(),
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta))
dim(alsfrs_total_3mo_meta)

# estimate alsfrs slope with linear regression, 
# excluding subjects with only one datapoint 
temp = alsfrs_total_3mo_meta
subject.one.datapoint = temp[temp$n==1,]$SubjectID
temp2 = alsfrs_total_3mo[!(alsfrs_total_3mo$SubjectID %in% 
                             subject.one.datapoint),]

# Apply linear regression by SubjectID
###
lm = with(temp2, by(temp2, SubjectID, 
                       function(x) lm(ALSFRS_Total ~ feature_delta, 
                                      data=x)))

alsfrs_total_slope = sapply(lm, coef)[2,]
alsfrs_total_slope_tab = as.data.frame(alsfrs_total_slope)
alsfrs_total_slope_tab$SubjectID = rownames(alsfrs_total_slope_tab)
# Merge fvc.meta and slope.tab
alsfrs_total_3mo_meta_slope = merge(alsfrs_total_slope_tab, 
                               alsfrs_total_3mo_meta, 
                               by="SubjectID", all.y = T)
write.csv(alsfrs_total_3mo_meta_slope, "alsfrs_total_3mo_meta_slope.csv", 
          quote=F, row.names = F)

# ALSFRS_R_Total meta-features and slope during the first 3 mo 
alsfrs_r_total_3mo = alsfrs_revised_wide %>%
  filter(feature_delta >=0 & feature_delta < 92) %>%
  select(SubjectID, ALSFRS_R_Total, feature_delta)

alsfrs_r_total_3mo_meta = alsfrs_r_total_3mo %>%
  group_by(SubjectID) %>% 
  summarise(mean_alsfrs_r_total = mean(ALSFRS_R_Total), 
            min_alsfrs_r_total = min(ALSFRS_R_Total), 
            max_alsfrs_r_total = max(ALSFRS_R_Total), 
            n=n(),
            first = first(feature_delta), 
            last = last(feature_delta), 
            interval = last(feature_delta) - first(feature_delta))
dim(alsfrs_r_total_3mo_meta)

# estimate alsfrs slope with linear regression, 
# excluding subjects with only one datapoint 
temp = alsfrs_r_total_3mo_meta
subject.one.datapoint = temp[temp$n==1,]$SubjectID
temp2 = alsfrs_r_total_3mo[!(alsfrs_r_total_3mo$SubjectID %in% 
                             subject.one.datapoint),]

# Apply linear regression by SubjectID
###
lm = with(temp2, by(temp2, SubjectID, 
                    function(x) lm(ALSFRS_R_Total ~ feature_delta, 
                                   data=x)))

alsfrs_r_total_slope = sapply(lm, coef)[2,]
alsfrs_r_total_slope_tab = as.data.frame(alsfrs_r_total_slope)
alsfrs_r_total_slope_tab$SubjectID = rownames(alsfrs_r_total_slope_tab)
# Merge fvc.meta and slope.tab
alsfrs_r_total_3mo_meta_slope = merge(alsfrs_r_total_slope_tab, 
                                    alsfrs_r_total_3mo_meta, 
                                    by="SubjectID", all.y = T)
write.csv(alsfrs_r_total_3mo_meta_slope, "alsfrs_r_total_3mo_meta_slope.csv", 
          quote=F, row.names = F)


# The End # 
