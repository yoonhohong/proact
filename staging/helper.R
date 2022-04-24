
library(dplyr)

# if both Q1 and Q3 item scores are 4, then assign stage 0.   
# if either Q1 or Q3 item score is 1 or less, then assign stage 2.   
# otherwise assign stage 1.  

convert_bulbar = function(x){
  y = case_when(
    all(x == 4) ~ 0,
    any(x <= 1) ~ 2,
    TRUE ~ 1)
  y
}

# if all item scores of motor domain (Q4-9) are 4, then assign stage 0.   
# if two or more item scores are 1 or less, then assign stage 2.
# otherwise assign stage 1.  

convert_motor = function(x){
  y = case_when(
    all(x == 4) ~ 0,
    sum(x %in% c(0,1)) >= 2 ~ 2,
    TRUE ~ 1)
  y
}

# if all respiratory items (Q10-12) scores are 4, then assign stage 0.   
# if any item score is 1 or less, then assign stage 2.   
# otherwise assign stage 1.  

convert_resp = function(x){
  y = case_when(
    all(x == 4) ~ 0,
    any(x <= 1) ~ 2,
    TRUE ~ 1
  )
}

# 
collapse1 = function(x){
  y = case_when(
    x %in% c(0,1) ~ 0,
    x %in% c(2,3) ~ 1,
    TRUE ~ 2)
  y
}

collapse2 = function(x){
  y = case_when(
    x == 0 ~ 0,
    x %in% c(1,2,3) ~ 1,
    TRUE ~ 2)
  y
}


