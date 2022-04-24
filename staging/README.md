# ALS staging 

# ML-based Staging  

clinical staging   
**ALSFRS_staging.R**     
**ALSFRS_staging_EDA.R**     

datasets    
**Data files** 
https://www.dropbox.com/sh/3lmi5ii3sgyi7o3/AACeVLTGtSDXSKIaX6P7Hxj2a?dl=0  

**ALSFRS_rev_clinicalStage.csv**      
ALSFRS_rev, clinical stages, and feature_delta   
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
