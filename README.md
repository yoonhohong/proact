# Goals  
To preprocess PROACT dataset     

**Data files** 
You can find out the data files at the following url.    
https://www.dropbox.com/sh/3lmi5ii3sgyi7o3/AACeVLTGtSDXSKIaX6P7Hxj2a?dl=0  

or in the following folders.       
Dropbox/ALSmaster/PROACT/PROACT_rawdata   
Dropbox/ALSmaster/PROACT/PROACT_preprocessed       

# Data source    
PRO-ACT database https://nctu.partners.org/proact   
The above data source was accessed in Sept. 2017 

DB contains data of 10,723 patients (including all datasets for training, training2, leaderboard, validation)  
4,456,146 records 


# Workflow 

**preprocess.R**    

## Preprocess 1 

1. Filtering   
- exclude *duplicate* records, records with *inconsistent* or  *errorneous* values (feature_delta < 0, diag_delta > 0, onset_delta > 0, diag_delta < onset_delta, both Q5a and Q5b filled-in)   
- exclude patients with *incomplete* ALShx or ALSFRS-R all item scores      

## Preprocess 2 

2. Creating meta-features (mean and slope values)  
- calculate mean value for time-varying features (ALSFRS_R_Total and item  scores, fvc, bmi, vital sign, and lab)     
- estimate slope for ALSFRS_R_Total, subdimension (bulbar, motor, respiration), and fvc over the first 3 mo, using linear model (time interval between the first and last measurements should be > 1.5 mo & the number of measurements should >= 2)       
- calculate onset2dx (onset to diagnosis)   

3. Transforming features   
- convert the sign of onset_delta and diag_delta from minus to plus    
- convert the unit of feature_delta, onset_delta and diag_delta (days to months)         
- combine the categories of onset_site (Limb, Limb and Bulbar, and Others) into Nonbulbar    
- create a new feature "Gastrostomy" (True/False) 
- convert Q5a/Q5b items into Q5   

4. Preprocessed data files  
- Predicting features data *PROACT_preprocessed_rev.csv*   
- Target data: Loss of functional autonomy in swallowing *LOA_swallowing.csv*    



# Imputation   

**merge_predictors_target.R**   
merge data (predictors and target)   
- target data   
- predictors: static and meta-features over the first 3 mo    
- evaluate missing data pattern and proportion: *missing_prop_pattern.png*   
- create a merged file (for missing value imputation) *merge_predictors_target.csv*      

**impute4slope_rev.R**    
- impute missing values in *merged_slope_rev.csv*   
- multiple imputation using mice package    
- exclude features with missing data proportion > 30 % except for bmi_mean   
- data distribution before imputation: *distribution_data.png*    
- missing data pattern: *missing_pattern.png*   
- check convergence following multiple imputation: *convergence.png*  
- check imputed value density plot (continuous features): *imputation_densityPlot.png*   
- check imputed value proportion plot (categorical feature, riluzole in this case): *imputation_riluzole.png*   
- save imputed dataset object: *imputed_rev.RDS*   


# Feature selection   


# Prediction  





