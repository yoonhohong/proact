# Goals  

- To develop a personalized prediction model for disease progression/survival and major events (loss of autonomy in major functional domains, eg. gastrostomy, NIV, etc) in ALS   

- To develop ML-based disease staging of ALS (HMM)       

- To identify distinct subgroups of disease progression trajectory (clustering of panel data)    

**Target variables**    

- ALSFRS (orig or rev) total score slope during 3-12 months (including all datasets for training, training2, leaderboard, validation)  

- Survival (including all datasets for training, training2, leaderboard, validation)  

- Gastrostomy  

**Note**    
ALSFRS rev version    
https://www.encals.eu/wp-content/uploads/2016/09/ALS-Functional-Rating-Scale-Revised-fill-in-form.pdf   

ALSFRS orig version    
https://www.sralab.org/sites/default/files/2017-07/PMandR_ALSRatingScale033111.pdf   


**R scripts**  
https://github.com/yoonhohong/PROACT  
(local repository) /Users/hong/Documents/GitHub/PROACT  

**Data files** 
https://www.dropbox.com/sh/3lmi5ii3sgyi7o3/AACeVLTGtSDXSKIaX6P7Hxj2a?dl=0  
(local) /Users/hong/Dropbox/ALSmaster/PROACT 


# Preparations  
## Data source  

PRO-ACT database  
https://nctu.partners.org/proact   

Accessed Sept. 2017 
DB contains data of 10,723 patients (including all datasets for training, training2, leaderboard, validation)        

# Workflow 

**preprocess.R**    
data preprocessing and merging for either ALSFRS orig or rev    

1. filtering   

- excluded duplicate records, inconsistent values, and 
errorneous records (feature_delta < 0)   
- excluded either none or incomplete ALSHX data 
(diag_delta, onset_delta, onset_site)  
- excluded erroneous values (diag_delta > 0, onset_delta > 0, 
diag_delta < onset_delta)  
- excluded either none or incomplete ALSFRS orig scores data(or ALSFRS rev scores)   
- excluded errorneous records (both Q5a and Q5b filled-in)  

2. create aggregated and meta-features  

- estimate slope for ALSFRS total scores, bulbar scores, motor scores, respiration scores, fvc and svc over the first 3 mo (interval should be > 1.5 mo, number of measures >= 2), using linear model       
- calculate mean value for ALSFRS total (and item) scores, fvc, svc, bmi, vital sign, and lab   
- calculate onset2dx   

3. transform features   

- convert the sign of onset_delta and diag_delta from minus to plus    
- convert the unit of feature_delta, onset_delta and diag_delta  from days to months     
- combine the categories of onset_site (Limb, Limb and Bulbar, and Others) into Nonbulbar    
- create new feature "Gastrostomy" (True/False) and replace Q5a and Q5b items with Q5   
- replace Q10_Respiratory item score with respiratory dimension score (d/t many NAs in Q10_Respiratory item)    


4. create merged files for static and meta-features data  

- *PROACT_preprocessed_orig.csv*   
- *PROACT_preprocessed_rev.csv*   

5. create files for longitudinal data 

- *ALSFRS_orig.csv*    
- *ALSFRS_rev.csv*   
- *fvc.csv*   
- *svc.csv*    
- *weight.csv*   
- *vitalsign.csv*    
- *lab.csv*    


**survival_target.R**     
- survival (including all datasets for training, training2, leaderboard, validation) (n=9080 patients)    
- convert days to months in time_event   

**slope_orig_target.R**  
- ALSFRS total score slope during 3-12 months (including all datasets for training, training2, leaderboard, validation) (n=3096 patients)     

**slope_rev_target.R**  
- ALSFRS rev total score slope during 3-12 months    
- exclude patients with ALSFRS rev scores between 3 mo and 12 mo not available   
- exclude patients with only one ALSFRS rev scores    available or interval between the first and last records <= 5 mo     
- *ALSFRS_rev_slope_target.csv*


**respiration.R** 
- to investigate the relationship between ALSFRS rev respiration item scores and FVC/SVC over the first 3 months after enrollment   


# Imputation   

**merge4slope_rev.R**   
merge data (target variable and predictors)   
- target variable: slope_3to12  *ALSFRS_rev_slope_target.csv*
- predictors: static and meta-features over the first 3 mo, *PROACT_preprocessed_rev.csv*   
- evaluate missing data pattern and proportion: *missingData.png*   
- create a merged file for missing value imputation: *merged_slope_rev.csv*   

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
- using Boruta package   







# Progression prediction (slope of ALSFRS total score)  

Goal: build a model to predict the slope of ALSFRS total scores     
Preprocessing: scaling (w/ standard deviation and mean centering)    
Algorithm: linear regression (lasso), random forest      

Results:  
Correlation plot between observed vs. predicted slope values    
![scatter_plot_slope_obs_pred](/images/cor_lm_rf.png)     

Comparisons of model performance: MAE, RMSE, Rsquared
![model_comparison](/images/model_comparisons.png)   

found no significance in paired t-test w/ bonferroni correction  
but there was a significant difference in Rsquared in raw p-value      



# Survival prediction  





# Clustering (longitudinal)  
based on ALSFRS trajectory (item scores or dimension scores)     
optimal number of clusters?     
onset_site    
differentiate between local progression rate and spread (to adjacent region) rate...       


# ML-based Staging  

clinical staging   
**ALSFRS_staging.R**     
**ALSFRS_staging_EDA.R**     

datasets    
**ALSFRS_orig_clinicalStage.csv**      
ALSFRS_orig, clinical stages, and feature_delta   
**ALSFRS_rev_clinicalStage.csv**      
ALSFRS_rev, clinical stages, and feature_delta   
**ALS_orig_clinicalStage.csv**     
ALS clinical staging and feature_delta    
**ALS_rev_clinicalStage.csv**     
ALS clinical staging and feature_delta    

HMM model   
hidden state: disease stages      
observable states: ALSFRS_orig or ALSFRS_rev scores (item scores or dimension scores)      

=======
## ML-based Staging  
algorithm: HMM model (or can try RNN)     
- hidden state: disease stages      
- observed variables: ALSFRS_orig or ALSFRS_rev scores (item scores or dimension scores)      


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


