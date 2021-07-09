

# Goals  

- To develop personalized progression/survival prediction models for patients with ALS   
- To develop ML-based subtyping (clustering)/staging of ALS patients  

**Target variables**    
- ALSFRS total score slope during 3-12 months (including all datasets for training, training2, leaderboard, validation)  
(n=3096 patients)    
- Survival (including all datasets for training, training2, leaderboard, validation)  
(n=9080 patients)   

**R scripts**  
https://github.com/yoonhohong/PROACT  
(local repository) /Users/hong/Documents/GitHub/PROACT  

**data files** 
https://www.dropbox.com/sh/3lmi5ii3sgyi7o3/AACeVLTGtSDXSKIaX6P7Hxj2a?dl=0
(local) /Users/hong/Dropbox/ALSmaster/PROACT  

# Preparations  

## Data source  

PRO-ACT database  
https://nctu.partners.org/proact   

Accessed Sept. 2017 
DB contains data of 10,723 patients (including all datasets for training, training2, leaderboard, validation)        

## Scripts   
**preprocess.R**  
**slope_target.R**  
- ALSFRS total score slope during 3-12 months (including all datasets for training, training2, leaderboard, validation) 
(n=3096 patients)     
**survival_target.R**     
Survival (including all datasets for training, training2, leaderboard, validation)   
(n=9080 patients)   
**respiration.R** 
- to investigate the relationship between ALSFRS revised respiration item scores and FVC and SVC measures over the first 3 months after enrollment   

## Data files  
**ALSFRS_slope.csv**  
- ALSFRS original version 
- slope over the 3 - 12 mo  
**survival.csv**   
- survival outcomes 
**demographic.csv**     
- include data with complete demographic informtion   
(n=8,646 patients) 
**ALSFRS**  
- exclude exact duplicates  
- exclude alsfrs records with feature_delta < 0  
- exclude ALSFRS records with the same feature_delta but having different feature_value (partial duplicates)  
- exclude records with both Q5a and Q5b item scores 
- create new feature named "Gastrostomy" (True/False)  
- delete Q5a and Q5b items with Q5 item left  
- separate the whole data into two sets (i.e, original vs. revised) 
**ALSFRS_original.csv**   
- NAs in Q10_Respiratory are probably errors in filling in the Q10_Respiratory...so the Q10_Respiratory item scores are replaced with respiratory dimension score (respiratory) 
- select records with complete item scores information   
- 58,412 records in 6,377 patients (ALSFRS original) during 0 to 2,114 days 
**ALSFRS_revised.csv**    
- select records with complete item scores information  
- 29381 records in 3275 patients (ALSFRS revised) during 0 to 2,114 days  
**alsfrs_orig_3mo_meta_slope.csv**
**alsfrs_revised_3mo_meta_slope.csv**
- min, max, mean   
- number of measurements, interval, first, last  
- linear regression slope estimate (only cases with interval >= 8 wks)  
**fvc.csv**  
- exclude feature delta < 0  
- exclude duplicates (full)
- exclude duplicates (partial)   
- exclude missing values   
- 44,516 records in 7,313 patients  
- 0 to 2,120 days  
**fvc_3mo_meta_slope.csv**
- fvc_3mo
- 19,660 records in 7218 patients   
- meta features; mean, max, mean  
- number of measurements, interval, first, last      
- linear regression slope estimates (only cases with interval >= 8 wks)   
**svc.csv**   
- exclude full duplicates 
- no partial duplicates 
- 4,826 records in 695 patients    
**svc_3mo_meta_slope.csv**  
- svc_3mo  
- 2,056 records in 695 patients   
- metafeatures; mean, max, mean   
- linear regression slope estimates (only in cases with interval >= 8 wks)    
**als_hx.csv**   
- exclude records with diag_delta > 0 or onset_delta > 0 (which means that enrollment precedes diagnosis or symptom onset) 
- include records with full information (excluding a case with missing value in onset_site)   
- 4,453 patients      
**family_hx.csv**
- no duplicates    
- 1,007 patients (Yes in 131 patients) 
**riluzole.csv**  
- no duplicates 
- 8,817 patients (Yes in 6,823 patients)    
**treatment_group.csv**  
- no duplicates   
- 9,640 patients (Active in 6728 patients)  
**bmi.csv**
**weight.csv**
- exclude duplicates (full and partial)
- BMI 6337 records in 4550 patients (BMI_3mo_mean, 4549 patients)    
- Wt 50,266 records in 7587 patients over 0 to 2114 days (Wt_3mo_mean, 4549 patients)   
**vitalsign.csv**  
- Vital signs, 61,237 records in 7,043 patients   
**vitalsign_3mo_meta.csv** 
- metafeatures; mean   
- 7024 patients    
**lab.csv**    
- exclude full and partial duplicates 
- feature_delta >= 0    
- 66,638 records in 7,776 patients 
**lab_3mo_mean.csv**   
- 3 mo mean in 7712 patients 


# Imputation
## Missing data: proportion and pattern  

multiple imputation using mice package    


# Progression prediction (slope of ALSFRS total score)  

Goal: build a model to predict the slope of ALSFRS total scores     

Features:    
- Age, Gender, onset_delta, diag_delta, onset2delta (meta feature, exclude cases with onset2delta <= 0)   
- ALSFRS (total score, item scores, three dimension - bulbar, motor, respiratory scores), FVC  
- calculate mean values over the first 3 months for the time-resolved features (ALSFRS, FVC)    
- ALSFRS Q5a and Q5b item scores were excluded, and instead made a new feature (gastrostomy) based on these features    
- ALSFRS bulbar dimension score did not include Q2_salivation because of the symptomatic treatments available       
- not included onset_site because of a large proportion of missing values (corresponding information could be inferred from the ALSFRS item or dimension scores)           
- exclude cases with missing values in onset_delta because of a large proportion of missing values (exactly the same cases with missing values in diag_delta)    

Preprocessing: scaling (w/ standard deviation and mean centering)    

Algorithm: linear regression (lasso), random forest      

Results:  
Correlation plot between observed vs. predicted slope values    
![scatter_plot_slope_obs_pred](/images/cor_lm_rf.png)     

Comparisons of model performance: MAE, RMSE, Rsquared
![model_comparison](/images/model_comparisons.png)   

found no significance in paired t-test w/ bonferroni correction  
but there was a significant difference in Rsquared in raw p-value      

mutual information...   


# Survival prediction  


# Clustering (cross-sectional)


# Clustering (longitudinal)  
based on ALSFRS trajectory (item scores or dimension scores)     
optimal number of clusters?     
onset_site    
differentiate between local progression rate and spread (to adjacent region) rate...       



# ML-based Staging  
HMM model   
hidden state: disease stages      
observable states: ALSFRS_original or ALSFRS_revised scores (item scores or dimension scores)      
=======
## ML-based Staging  
algorithm: HMM model (or can try RNN)     
- hidden state: disease stages      
- observed variables: ALSFRS_original or ALSFRS_revised scores (item scores or dimension scores)      


item scores or dimension scores for the observed variables    

why not total scores? 
dimensionality analysis argues against the use of ALSFRS-R as a single score because the scale lacks unidimensionality.   
https://pubmed.ncbi.nlm.nih.gov/23516308/    

strategy for aggregating items for dimension scores    
based on the results of exploratory factor analysis, which represent 3 domains as followings:    
(1) bulbar function (Q1_Speech, Q2_Salivation, Q3_Swallowing);    
(2) fine and gross motor function (Q4_Handwriting, Q5a_Cutting_without_Gastrostomy/Q5b_Cutting_with_Gastrostomy, Q6_Dressing_and_Hygiene, Q7_Turning_in_Bed, Q8_Walking, Q9_Climbing_Stairs); and  
(3) respiratory function (Q10_Respiratory).    

we also need to consider this...   
collapsing the scale's 5 level ratings into 3 levels improved its metric quality       

convert (estimate) ALSFRS or ALSFRS-R score into clinical stage, and use this to construct the initial transition probability matrix for HMM.   
there are two clinical staging systems, e.g., King's staging and the MiTOS staging    
King's staging is based on the number of **neuroanatomical regions** involved (bulbar, cervical, lumbosacral) and the need for gastrostomy and non-invasive ventilation.    
MiTOS staging is based on the number of **functional domains** where the patient lose autonomy (swallowing, communication, movement, breathing)    
references:    
Estimating clinical stage of amyotrophic lateral sclerosis from the ALS Functional Rating Scale https://pubmed.ncbi.nlm.nih.gov/24720420/    
Clinical staging in amyotrophic lateral sclerosis: analysis of Edaravone Study 19 https://jnnp.bmj.com/content/92/2/165    

number of stages: 5 (arbitrary)   
uneven time data (interval, whole follow-up duration)   
can first try last observation carried forward, and later elaborate on this issue... 

specific questions to be addressed      
- can we estimate disease stage at specific time point (e.g., at enrollment) w/o the whole sequence of observations?      
- comparison of HMM stages with King's and the Mito's stages    


