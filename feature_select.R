
# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

library(tidyverse)
library(Boruta)

df = readRDS("imputed_rev.RDS")

boruta_output <- Boruta(slope_3to12 ~ ., data=df, 
                        doTrace=1)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, 
                                       withTentative = TRUE)
print(boruta_signif)  

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', 
             c('meanImp', 'decision')]
imps2[order(-imps2$meanImp), ] # descending sort

# Plot variable importance
par(mar = c(13, 5, 5, 2))
plot(boruta_output, las=2, 
     xlab="", main="Variable Importance")  # 8*8 inches
# The columns in green are ‘confirmed’ and 
# the ones in red are not. 
# There are couple of blue bars representing 
# ShadowMax and ShadowMin. 
# They are not actual features, 
# but are used by the boruta algorithm to decide 
# if a variable is important or not.

imp_features = row.names(imps2)
df1 = df %>%
  select(all_of(imp_features), slope_3to12)

saveRDS(df, file = "slope_rev_selected_features.RDS")




