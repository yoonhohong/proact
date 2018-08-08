# Read proact data, preprocessed and cleaned 

proact = read.csv("PROACT_preprocessed_cleaned.csv")

# Imputation
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

for (var in 1:ncol(proact)) {
  if (class(proact[,var]) %in% c("numeric", "integer")) {
    proact[is.na(proact[,var]),var] <- mean(proact[,var], na.rm = TRUE)
  } else if (class(proact[,var]) %in% c("character", "factor")) {
    proact[is.na(proact[,var]),var] <- Mode(proact[,var], na.rm = TRUE)
  }
}

temp = sapply(proact, class)
temp[temp %in% c("factor", "character")]

# Dummy variables: 
# Gender, Race, onset_site, family_ALS_hist, if_use_Riluzole, treatment_group

library(dummies)
proact.dummy = dummy.data.frame(proact)
library(dplyr)
proact.dummy %>%
  select(-c(GenderF,RaceUnknown, onset_siteOther, family_ALS_histN, if_use_RiluzoleNo,
            treatment_groupPlacebo)) -> proact.dummy.sub

write.csv(proact.dummy.sub, "PROACT_imputed_dummied.csv", quote=F, row.names=F)
