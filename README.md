# Workflow 

Predicting model for ALS...
progression (ALSFRS slope) 
classification (fast, medium, slow progressors)
survival 
loss of autonomy in physical functions (MITOS stages)
...
PRO-ACT database 

Accessed Sept. 2017 
DB contains 10,723 patients 
full demographic data (n=6514)
survival data (n=9080)
clinical trials, how many? 
period? 

Preprocess1.R
: full demographic data (n=6514)
: at least one complete ALSFRS or ALSFRS-R records with feature_delta >= 0 (n=6507)
: convert ALSFRS-R into ALSFRS 
: calculate Mitos staging 
: ALSFRS_MITOS_all.csv

Preprocess2.R (optional)
: exclude left censoring in any functional domains (within the first 3 months)
: ALSFRS_excl_leftcensored.csv, MITOS_excl_leftcensored.csv

Mitos_survival.R
: survival data (n=9080)
: KM estimates for loss of autonomy in each functional domain 
: KM estimates for surivival 
: Figures (KM curves) 
: Survival_all.csv

Datacleaning.R
: cleaning data including demographic, clinical history, functional scales, lab, vital signs, riluzole, treatment, etc. 
: extract meta features (min, max, mean, slope) for functional scales, otherwise mean values for time-resolved features 
: Demographic.csv, FVC_meta.csv, ASLFRS_meta.csv, ALShx.csv, FamilyHx.csv, Rluzole.csv, SVC_meta.csv, Treatment.csv, Vitals.csv, Lab.csv
: PROACT_preprocessed_cleaned.csv

Calculate_ALSFRS_slope.R
: Slope calculation target patients (n=5521)
: Calculate ALSFRS slope 3-12 mo 
: Plot ALSFRS trajectory of 4 quartile subgroups categorized by ALSFRS slope 3-12 mo
: Examine correlation btw initial 3 mo slope and 3-12 mo slope 

Impute_dummy.R
: impute with mean and mode 
: dummy variables 
: PROACT_imputed_dummied.csv

Predict_ALSFRS_slope.R
: predict ALSFRS_Total slope during 3-12 month period 
: by using baseline 3 mo data
: linear regression (lasso), random forest

Predict_ALSFRS_class.R
: predict AlSFRS class during 3-12 mo
: with baseline 3 mo data
: fast, medium, slow (medium; IQR, fast: 1st quartile, slow; 4th quartile)
: logistic regression (lasso), random forest 

Predict_survival.R
: coxph, random forest 
: prediction error curve 

Predict_MITOS.R
: coxph
: prediction error curve 
: clustering




##########################

"CV_cox", "CV_coxboost", "CV_randomforest" are for making models and validating the performances by cross-validation. Each one uses cox, coxboost, randomforest, respectively.

"Variable_importance" is for calculating variable importance and plotting examples of predicted time-to-event curves.

"Clustering" is for integrating 5 models' results and clustering patietns based on probability at 12 month in 5 domains. It is ongoing.
