library(tidyverse)
library(mice)

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT") 

df = read.csv("merged_slope_rev.csv")

# deselect SubjectID, slope_3to12 
df_pred = df %>%
  select(-c(SubjectID, slope_3to12)) 

df_target = subset(df, select = "slope_3to12")

# imputation 
df_imputed = mice(df_pred, maxit = 10, m = 5, seed = 9)
df_imputed$loggedEvents # multi-collinearity 

# check convergence 
# check if there is more variation between the chains 
# than within each chain
# check if there is any chain that behaves differently from others 
temp = which(df_imputed$nmis > 0)
length(temp) # 17
plot(df_imputed, layout = c(6,6)) # 34, 12*12 inch   

# imputed data distribution 
densityplot(df_imputed) # 10 * 8 inch 

# a list of imputed data (length = 5)
imp_ls = complete(df_imputed, action = "all") 
length(imp_ls)
df_imputed_set1 = imp_ls[[1]]

df = cbind(df_target, df_imputed_set1)
saveRDS(df, file = "imputed_rev.RDS")



