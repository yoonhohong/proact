# Read alsfrs and mitos staging data (see Preprocess1.R)
# At least one complete ALSFRS or ALSFRS_R records
# ALSFRS_R converted to ALSFRS

alsfrsfull = read.csv("ALSFRS_MITOS_all.csv")

# Subjects who already lost functional autonomy...
# in at least one domain within the first 3 months
# Exclude...
alsfrsfull %>%
  filter((feature_delta < 92) & (ALSMITOS >= 1)) -> censor_left
unique(censor_left$SubjectID) -> censor_left_subject  
length(censor_left_subject) # 2219 patients
alsfrsfull %>%
  filter(!(SubjectID %in% censor_left_subject)) -> alsfrs_final
dim(alsfrs_final) # 40285 complete ALSFRS or ALSFRS-R records 
length(unique(alsfrs_final$SubjectID)) # 4288 patients

# save alsfrs_final data 
# summary of preprocessing...
# full demographic, complete ALSFRS or ALSFRS_R, not developed LOF in any functional 
# domains within the first 3 month

alsfrs_final %>%
  select(-c(Movement, Swallowing, Communicating, Breathing, ALSMITOS)) -> alsfrs.save
alsfrs_final %>%
  select(c(SubjectID, feature_delta, Movement, Swallowing, Communicating, Breathing, 
           ALSMITOS)) -> mitos.save

write.csv(alsfrs.save, "ALSFRS_excl_leftcensored.csv", quote = F, row.names = F)
write.csv(mitos.save, "MITOS_excl_leftcensored.csv", quote = F, row.names = F)

############################################################################
