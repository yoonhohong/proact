library(randomForestSRC)
library(ggRandomForests)
library(dplyr)
library(ggplot2)

data(pbc)

pbc %>% mutate(years = days/365, treatment = factor(treatment)) %>%
  select(-days) -> pbc
levels(pbc$treatment) = c("DPCA", "placebo")

# Data exploration 
# Find outliers, missing values and patterns, and other data anomalies (i.e., non-physiological values)...
# Categorical vs. continuous variables...
# Missing data proportion 
# Imputation

# KM estimates 
pbc.trial <- pbc %>% filter(!is.na(treatment))
pbc.test <- pbc %>% filter(is.na(treatment))

# Get KM estimates: gg_survival 
gg_dta = gg_survival(interval = "years",
            censor = "status", # should be numeric, not factor
            by = "treatment", # optional argument 
            data = pbc.trial, 
            conf.int = 0.95)

plot(gg_dta) + 
  labs(y="Survival Probability", x="Observation Time (years)", 
       color = "Treatment", fill = "Treatment") + 
  theme(legend.position = c(0.2,0.2)) + 
  coord_cartesian(y = c(0,1.01))

plot(gg_dta, type = "cum_haz") + 
  labs(y="Survival Probability", x="Observation Time (years)", 
       color = "Treatment") + 
  theme(legend.position = c(0.2,0.8)) + 
  coord_cartesian(y = c(0,1))

pbc.bili <- pbc.trial 
pbc.bili$bili_grp <- cut(pbc.bili$bili, breaks = c(0, 0.8, 1.3, 3.4, 29))

plot(gg_survival(interval = "years", censor = "status", by = "bili_grp",
                   data = pbc.bili), error = "none") +
  labs(y = "Survival Probability", x = "Observation Time (years)",
            color = "Bilirubin")

# Random forest: survival 

# Fitting random survival forest
# rfsrc
# Return random survival forest object
rfsrc_pbc = rfsrc(Surv(years, status) ~ ., data = pbc.trial, ntree = 1000, 
      nsplit = 10, importance = T, na.action = "na.impute",
      tree.err = T) # adaptive tree imputation 

# Out-of-Bag prediction error rates
# gg_error 
# Return OOB prediction error rates
# Concordance index error rates
plot(gg_error(rfsrc_pbc))

# Out-of-Bag prediction estimates
# gg_rfsrc 
# Return prediction estimates 
# In case of training set, OOB prediction estimates
# In case of test set, prediction estimates in the test set (as below)
ggRFsrc <- plot(gg_rfsrc(rfsrc_pbc), alpha = 0.2) +
  scale_color_manual(values = pbc.trial$status + 1) + 
  theme(legend.position = "none") + 
  labs(y = "Survival Probability", x = "Time (years)") + 
  coord_cartesian(ylim = c(-0.01, 1.01)) 

show(ggRFsrc)

ggRFsrc <- plot(gg_rfsrc(rfsrc_pbc, by = "treatment")) + 
  # bootstrap conf int 0.95, number of bs = number of observations
  theme(legend.position = c(0.2,0.2)) + 
  labs(y = "Survival Probability", x = "Time (years)") + 
  coord_cartesian(ylim = c(-0.01, 1.01)) 

show(ggRFsrc)

# Predict in test dataset 
# predict 
rfsrc_pbc_test = predict(rfsrc_pbc, newdata = pbc.test, na.action = "na.impute", 
                         importance = T)
rfsrc_pbc_test
plot(gg_rfsrc(rfsrc_pbc_test), alpha=.2) + 
  scale_color_manual(values = strCol) + 
  theme(legend.position = "none") + 
  labs(y = "Survival Probability", x = "Time (years)") + 
  coord_cartesian(ylim = c(-0.01, 1.01))

# Variable selection 
# Permutation, difference in error rates, VIMP
# gg_vimp 
plot(gg_vimp(rfsrc_pbc)) + 
  theme(legend.position = c(0.8, 0.2)) + 
  labs(fill = "VIMP > 0")

# Minimal depth 
# Averaging the depth of the first split for each variable over all trees within the forest
# var.select (randomForestSRC)
# gg_minimal_depth 

varsel_pbc = var.select(rfsrc_pbc)
gg_md = gg_minimal_depth(varsel_pbc)
print(gg_md)

# depth threshold: mean of the minimal depth distribution, 
# classifying variables with minimal depth lower than this threshold 
# as important in forest prediction

# model size: number of variables with depth below that threshold

plot(gg_md)

plot(gg_minimal_vimp(gg_md))



