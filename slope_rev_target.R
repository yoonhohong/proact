# alsfrs revised total score slope estimation 

library(dplyr)
library(tidyverse)
library(broom)

# import dataset 
alsfrs = read.csv("ALSFRS_revised.csv")
length(unique(alsfrs$SubjectID)) # 3059 patients 

# filter, 3-12 mo data 
alsfrs_3to12 = alsfrs %>%
  filter(feature_delta >3) %>%
  filter(feature_delta <= 12)
length(unique(alsfrs_3to12$SubjectID)) # 2757 patients 

# group by SubjectID
alsfrs_gr = alsfrs_3to12 %>%
  group_by(SubjectID) %>%
  arrange(by_group = T) %>%
  nest()

# exclude patients, if only one alsfrs record available, 
# if interval between the first and last records < 6
temp = alsfrs_gr %>%
  mutate(n = map_int(data, ~nrow(.x))) %>%
  mutate(first = map_dbl(data, ~first(.x$feature_delta))) %>%
  mutate(last = map_dbl(data, ~last(.x$feature_delta))) %>%
  mutate(interval = last - first)
temp2 = temp %>%
  filter(n > 1) %>%
  filter(interval > 5)
length(unique(temp2$SubjectID)) # 2025 patients 

# fit linear model, extract slope esimates 
temp3 = temp2 %>%
  mutate(model = map(data, ~lm(ALSFRS_R_Total ~ feature_delta, 
                            data = .x))) %>%
  mutate(statistics = map(model, ~tidy(.x))) %>%
  unnest(statistics) %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate) 
temp3 %>%
  ggplot(aes(estimate)) +
  geom_histogram()

temp4 = temp3 %>%
  mutate(estimate = round(estimate, 2)) %>%
  rename(slope_3to12 = estimate)

write.csv(temp4, "ALSFRS_rev_slope_target.csv", quote = F, row.names = F) 











