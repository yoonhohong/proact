# 데이터 폴더 안 파일 설명 

## data files 
- ALSFRS_rev.csv
- survival.csv
- stage_king_mitos.csv
- stage_bmr.csv 
- PROACT_preprocessed_rev.csv

## 각 파일의 fiels 설명   

**ALSFRS_rev** 

SubjectID - 환자 고유 ID 
feature_delta - 임상시험 등록으로부터 경과 시간(월 단위로 환산된) *
ALSFRS_R_Total - ALS functional rating scale revised version total score (아래 Q1 - Q9, R1,2,3 를 모두 합친 값)
ALSFRS_Total - ALS functional rating scale original version total score (아래 Q1 - Q10 를 모두 합친 값)
Q1_Speech - 구음 장애의 정도 
Q2_Salivation - 침 흘리는 정도          
Q3_Swallowing - 삼킴 장애의 정도 
Q4_Handwriting  - 손글씨쓰기 장애 정도             
Q5_Cutting  - 음식 자르기 장애 정도  
Q6_Dressing_and_Hygiene - 옷입기 위생 돌봄 장애 정도      
Q7_Turning_in_Bed - 침상에서 몸을 돌리기 장애 정도        
Q8_Walking - 걷기 장애 정도       
Q9_Climbing_Stairs - 계단 오르기 장애 정도       
Q10_Respiratory - 호흡 부전의 정도 
R1_Dyspnea - 숨쉬기 장애 정도         
R2_Orthopnea  - 기립성 호흡 부전의 정도           
R3_Respiratory_Insufficiency - 호흡 부전의 정도 
Gastrostomy - 위루술 여부 
bulbar - sum(Q1:Q3)  
motor - sum(Q4:Q9)
respiratory - sum(R1:R3) (or equal to Q10 in case of ALSFRS_orig.csv)     

[ALSFRS revised version](https://www.encals.eu/wp-content/uploads/2016/09/ALS-Functional-Rating-Scale-Revised-fill-in-form.pdf)

**survival**  

SubjectID - 환자 고유 ID 
time_event - 임상시험 등록으로부터 사망 혹은 추적 관찰 종류까지 경과 시간(월 단위로 환산함) 
status - 종료 시 상태 (0: censoring, 1: death)  


## HMM model   
hidden state: disease stages      
observable states: ALSFRS_rev scores (item scores or dimension scores)      

**why not total scores?**   
dimensionality analysis argues against the use of ALSFRS-R as a single score because the scale lacks unidimensionality.   
https://pubmed.ncbi.nlm.nih.gov/23516308/    

**strategy for aggregating items for dimension scores**     
based on the results of exploratory factor analysis, which represent 3 domains as followings:    
(1) bulbar function (Q1_Speech, Q2_Salivation, Q3_Swallowing);    
(2) fine and gross motor function (Q4_Handwriting, Q5a_Cutting_without_Gastrostomy/Q5b_Cutting_with_Gastrostomy, Q6_Dressing_and_Hygiene, Q7_Turning_in_Bed, Q8_Walking, Q9_Climbing_Stairs); and  
(3) respiratory function (Q10_Respiratory).    

we also need to consider the followings...   
collapsing the scale's 5 level ratings into 3 levels improved its metric quality       

estimate ALSFRS-R score into clinical stage, and use this to construct the initial transition probability matrix for HMM.   
there are two clinical staging systems, e.g., King's staging and the MiTOS staging    
 
King's staging is based on the number of **neuroanatomical regions** involved (bulbar, cervical, lumbosacral) and the need for gastrostomy and non-invasive ventilation.    
MiTOS staging is based on the number of **functional domains** where the patient lose autonomy (swallowing, communication, movement, breathing)    
**references**        
Estimating clinical stage of amyotrophic lateral sclerosis from the ALS Functional Rating Scale https://pubmed.ncbi.nlm.nih.gov/24720420/    
Clinical staging in amyotrophic lateral sclerosis: analysis of Edaravone Study 19 https://jnnp.bmj.com/content/92/2/165    


specific questions to be addressed      
- comparison of HMM stages with King's and the Mito's stages    







