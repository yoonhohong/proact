# Workflow 

## Goals  
- To develop personalized survival prediction model for patients with ALS   
- To develop subtype classification (clustering) of ALS patients  

## Dataset 
PRO-ACT database 

Accessed Sept. 2017 
DB contains 10,723 patients (including all datasets for training, training2, leaderboard, validation)
 

## preprocess.R

1. demographic dataset (n=8,653) 
-> demographic.csv 

2. ALSFRS dataset 
- duplicates excluded   
- separate data into two sets (i.e, original vs. revised)   
- 30,154 records with both ALSFRS original and revised 
- ALSFRS_original.csv    
- ALSFRS_revised.csv  

3. ALSFRS and ALSFRS_R total scores meta-features and slope during the first 3 months (>= 0 days & < 92 days)
- min, max, mean  
- number of measurements  
- time from enrollment to the first and to the last measurement
- interval between the first and last measurement   
- slope estimate by linear regression   
- alsfrs_total_3mo_meta_slope.csv
- alsfrs_r_total_3mo_meta_slope.csv

## survival.R
1. survival data (n=9080)   
- survival.csv 

## datacleaning.R
1. fvc_percent dataset  
- duplicates excluded (same/different duplicates)  
- fvc.csv

2. svc_percent dataset 
- duplicates excluded (same/different duplicates)  
- svc.csv 

3. fvc_percent (and svc_percent) meta-features during the first 3 months (>= 0 days & < 92 days) 
- min, max, mean  
- number of measurements  
- time from enrollment to the first and to the last measurement
- interval between the first and last measurement   
- slope estimate by linear regression   
- fvc_3mo_meta.csv  
- svc_3mo_meta.csv   

4. als hx   
- no duplicates   
- excluding records with diag_delta > 0 or onset_delta > 0
- als_hx.csv 

5. family hx 
- no duplicates   
- family_hx.csv 

6. riluzole 
- no duplicates  
- riluzole.csv 

7. treatment group
- no duplicates
- treatment_group.csv

8. vitals 
- exclude duplicates (same/different)
- bmi.csv
- weight.csv
- vitals.csv 

9. lab  
- duplicates excluded (same/different) 
- excluded records without feature_delta  
- lab.csv 


## Imputation
## Survival prediction 
## Clustering 

