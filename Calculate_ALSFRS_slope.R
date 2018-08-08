load("~/Documents/ALSmaster_data/Calculate_ALSFRS_slope.RData")
setwd("~/Documents/ALSmaster_data")

library(dplyr)
library(ggplot2)

# Calculate ALSFRS slope 3-12 mo 
# Plot ALSFRS trajectory of 4 quartile subgroups categorized by ALSFRS slope 3-12 mo
# Examine correlation btw initial 3 mo slope and 3-12 mo slope 

# ALSFRS_Total 
df = read.csv("ALSFRS_MITOS_all.csv")
df %>% select(SubjectID, feature_delta, ALSFRS_Total) -> alsfrs
# feature_delta days -> months
alsfrs$feature_delta = alsfrs$feature_delta/30

# Remove subjects with fewer than 2 visits within 3 months: alsfrs.sub
alsfrs %>%
  filter(feature_delta < 3) %>%
  group_by(SubjectID) %>%
  summarize(n = n()) %>%
  filter(n>1) -> alsfrs.temp
alsfrs %>%
  filter(SubjectID %in% alsfrs.temp$SubjectID) -> alsfrs.sub

# Slope ratio = [ALSFRS_Total(m2) - ALSFRS_Total(m1)] / (m2 - m1)
# m1 = the first visit after 3 mo
# m2 = the first visit after 12 mo or the last visit within 12 mo 

# The first visit after 3 mo; alsfrs.m1
alsfrs.sub %>%
  filter(feature_delta >= 3) %>%
  group_by(SubjectID) %>%
  mutate(rank = rank(feature_delta)) %>%
  filter(rank == 1) -> alsfrs.m1

# the first visit after 12 mo; alsfrs.m2
alsfrs.sub %>%
  filter(feature_delta >= 12) %>%
  group_by(SubjectID) %>%
  mutate(rank = rank(feature_delta)) %>%
  filter(rank == 1) -> alsfrs.m2

# if the first visit after 3 mo == the first visit after 12 mo -> remove 
alsfrs.m12 = rbind(alsfrs.m1, alsfrs.m2)
any(duplicated(alsfrs.m12))
dup = alsfrs.m12[duplicated(alsfrs.m12),]
alsfrs.m1 = subset(alsfrs.m1, !(SubjectID %in% dup$SubjectID))
alsfrs.m2 = subset(alsfrs.m2, !(SubjectID %in% dup$SubjectID))

# subjects who have the first visit after 3mo, but 
# no visit after 12 mo, set the last visit as m3
alsfrs.sub %>%
  filter(feature_delta >= 3) %>%
  filter(SubjectID %in% alsfrs.m1$SubjectID) %>%
  filter(!(SubjectID %in% alsfrs.m2$SubjectID)) %>%
  group_by(SubjectID) %>%
  mutate(rank = rank(feature_delta)) %>%
  filter(rank == max(rank)) -> alsfrs.m3

# if the first visit after 3 mo == the last visit before 12 mo -> remove 
alsfrs.m13 = rbind(alsfrs.m1, alsfrs.m3)
any(duplicated(alsfrs.m13))
dup = alsfrs.m13[duplicated(alsfrs.m13),]
alsfrs.m1 = subset(alsfrs.m1, !(SubjectID %in% dup$SubjectID))
alsfrs.m3 = subset(alsfrs.m3, !(SubjectID %in% dup$SubjectID))

# confirm none in intersect btw m2 and m3
intersect(alsfrs.m2$SubjectID, alsfrs.m3$SubjectID)
alsfrs.m23 = rbind(alsfrs.m2, alsfrs.m3)

# set dataset to calculate the slope: alsfrs.final
alsfrs.m123 = merge(alsfrs.m1, alsfrs.m23, by="SubjectID")
alsfrs.final = subset(alsfrs.sub, SubjectID %in% alsfrs.m123$SubjectID)

# calculate the slope: alsfrs.slope, ALSFRS_slope_after3mo.csv
alsfrs.final %>%
  filter(feature_delta >= 3) %>%
  group_by(SubjectID) %>%
  arrange(feature_delta) %>%
  summarise(alsfrs_first = first(ALSFRS_Total), alsfrs_last = last(ALSFRS_Total), 
         delta_first = first(feature_delta), delta_last = last(feature_delta)) %>%
  mutate(alsfrs_slope = (alsfrs_last - alsfrs_first)/(delta_last - delta_first)) -> alsfrs.slope

write.csv(alsfrs.slope, "ALSFRS_slope_after3mo.csv", quote=F, row.names=F)

# Plot: line plot grouped
# After removal of subjects with fewer than 2 visits within 3 months
# After removal of subects with fewer than 2 visits after 3 months
# categorize subjects into quartile subgroups according to the rate of progression 
q4 = quantile(alsfrs.slope$alsfrs_slope, probs = seq(0,1,0.25))
alsfrs.slope %>%
  mutate(subgroup = cut(alsfrs.slope$alsfrs_slope, breaks = q4, 
                  include.lowest = T, right = F)) -> alsfrs.slope
temp = merge(alsfrs.final, alsfrs.slope, by="SubjectID")
levels(temp$subgroup) = list(fast = "[-10.7,-1.26)", 
                             medium = c("[-1.26,-0.717)","[-0.717,-0.331)"), 
                             slow = "[-0.331,7.5]")

# 
ggplot(data = temp, aes(x=feature_delta, y=ALSFRS_Total, group=SubjectID, color = subgroup)) + 
  geom_line(alpha = 0.2) + # warning message pass 
  xlim(0,20) + 
  theme_bw() + 
  labs(x = "Time after enrollment (months)") + 
  facet_wrap(~subgroup, ncol=2)

# linear regression model: slope 
# slope within the first 3 mo after enrollment

# alsfrs within the first 3 mo 
alsfrs.final %>%
  filter(feature_delta < 3) -> alsfrs.lt3mo

lm = by(alsfrs.lt3mo, alsfrs.lt3mo$SubjectID, 
        function(x) lm(ALSFRS_Total ~ feature_delta, data=x))
slope1 = sapply(lm, coef)[2,]
slope.lt3mo = slope1[names(slope1) %in% alsfrs.slope$SubjectID]

# correlation btwn slope.lt3mo and alsfrs_slope
slope = as.data.frame(cbind(slope.lt3mo, alsfrs.slope))
ggplot(data = slope, aes(x=slope.lt3mo, y=alsfrs_slope)) + 
  geom_point(alpha=0.2) + 
  xlim(-5,2) + ylim(-5,2) + # 
  geom_smooth(method="lm", lty="dashed") + 
  theme_bw() + 
  labs(x="Slope within the first 3 mo after enrollment", 
       y="Slope for the next f/u period after enrollment", 
       title = "ALSFRS_Total") # correlation coef ? p-value ? 
cor.test(slope$slope.lt3mo, slope$alsfrs_slope)

range(slope$alsfrs_slope)
plot(density(slope$alsfrs_slope))
abline(v=q4[2], lty="dashed", col="blue")
abline(v=q4[3], lty="dashed", col="red")
abline(v=q4[4], lty="dashed", col="green")
