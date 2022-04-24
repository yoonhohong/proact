# Imputation 

# Merge PROACT_preprocessed_rev and LOA_swallowing     
# Missing data pattern and proportion 
# Imputation (multiple imputation using mice package)  

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# import library 
library(tidyverse)
library(mice)
library(visdat)

df_pred = read.csv("PROACT_preprocessed_rev.csv")
df_pred = df_pred %>%
  rename(riluzole = if_use_Riluzole)

df_target = read.csv("LOA_swallowing.csv")

df_pred2 = df_pred %>%
  filter(SubjectID %in% df_target$SubjectID) 
# 2670 patients with loss of autonomy in swallowing function data available (baseline; 3 mo after enrollment)  

# missing data pattern 
temp = df_pred2 %>%
  select(-SubjectID)
png("~/Documents/GitHub/PROACT/images/missing_prop_pattern.png", 
    width = 3000, height = 2000, 
    units = "px", res = 350)
p = vis_miss(temp, cluster = T, sort_miss = T) # clustering row-wise, sorting column-wise
p + theme(axis.text.x = element_text(angle = 90))
dev.off()

# exclude features with missing data proportion > 40% 
temp = df_pred2 %>%
  summarise_all(~sum(is.na(.x))/length(.x)) 
miss_lt40perc = which(temp < 0.4)

df_pred3 = df_pred2 %>%
  select(all_of(miss_lt40perc)) 

df_pred3$Race = factor(df_pred3$Race)
df_pred3 %>% 
  count(Race) %>%
  mutate(prop = n/sum(n)*100)

df_pred4 = df_pred3 %>% 
  select(-Race) # nearly constant (91%)

df_pred4 = within(df_pred4, {
  Gender = ifelse(Gender == "F", 0, 1)
  onset_site = ifelse(onset_site == "Nonbulbar", 0, 1)
  Gastrostomy = ifelse(Gastrostomy == T, 1, 0)})

df_pred4 = df_pred4 %>%
  select(-Gastrostomy)

df_pred4$riluzole = ifelse(is.na(df_pred4$riluzole), NA, 
                           ifelse(df_pred4$riluzole == "No", 
                                  0, 1))

# correlation between predictors 
temp = df_pred4 %>%
  select(-SubjectID)
png("~/Documents/GitHub/PROACT/images/correlation_predictors_plot.png", 
    width = 2400, height = 2000, 
    units = "px", res = 300)
p = vis_cor(temp)
p + theme(axis.text.x = element_text(angle = 90))
dev.off()

# data distribution before imputation 

df = df_pred4 %>%
  inner_join(df_target, by = "SubjectID")

df_item = df %>%
  select(Q1_Speech:R3_Respiratory_Insufficiency)

df_item2 = df_item %>%
  mutate_all(~cut(.x, breaks = c(0,1,2,3,4), 
                  include.lowest = T, right = T)) %>%
  mutate_all(~factor(.x, ordered = T))

df_others = df %>%
  select(-c(Q1_Speech:R3_Respiratory_Insufficiency)) %>%
  select(-SubjectID)

# Quickly get an overview of all variables,
# categorical and continuous

temp = df_item2 
temp = df_others %>%
  select(-c(event, feature_delta))

nc <- max(5, ceiling(sqrt(ncol(temp))))
nr <- ceiling(ncol(temp) / nc)

# df_item2
png("~/Documents/GitHub/PROACT/images/distribution_data_1.png", 
    width = 1300, height = 960, 
    units = "px")
# df_others 
png("~/Documents/GitHub/PROACT/images/distribution_data_2.png", 
    width = 1300, height = 960, 
    units = "px")

par(mfrow = c(nr, nc), 
    mgp = c(2, 0.6, 0), 
    mar = c(2, 3, 3, 0.5), 
    cex = 1.2)

for (i in 1:ncol(temp)) {
  if (is.numeric(temp[, i])) {
    hist(temp[, i], nclass = 50, xlab = "",
         main = names(temp[i])
    )
  } else {
    barplot(table(temp[, i]), ylab = "Frequency",
            main = names(temp[i]))
  }
}

dev.off()

dim(df) # 2670 patients 

# save 
write.csv(df, "merged_predictors_target.csv", quote = F, row.names = F)

