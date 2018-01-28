library(impute) 
allpatients <- data.frame("SubjectID"=unique(alsfrsfull$SubjectID))
allpatientsfeature <- merge(allpatients,fullfeature,all.x=TRUE)

# find variables of which missing <30% and choose them
missing<-vector()
for( i in names(allpatientsfeature)){
  missing[i]=sum(is.na(allpatientsfeature[[i]]))/length(unique(allpatientsfeature$SubjectID))
}
manyvariables=missing[missing<0.3]
allpatientsfeature_manyvariables <- allpatientsfeature[,names(manyvariables)]

# divide variables into categorical and numerical
imputedcategorical=allpatientsfeature_manyvariables[,c("Race","onset_site","Gender","if_use_Riluzole","treatment_group","MITOS","KINGS","if_R")]
numericalfeatures=setdiff(names(manyvariables),c("SubjectID","Race","onset_site","Gender","if_use_Riluzole","treatment_group","MITOS","KINGS","if_R"))

# Make 'Mode' function
Mode <- function(x){
  x <- x[!is.na(x)]
  uqx <- unique(x)
  uqx[which.max(tabulate(match(x,uqx)))]
}


# Impute categorical variable by Mode
for(i in c("Race","onset_site","Gender","if_use_Riluzole","treatment_group","MITOS","KINGS","if_R")){
  imputedcategorical[i][is.na(imputedcategorical[i])]=Mode(imputedcategorical[i])
}

# Impute numerical variable by KNN
imputednumericalmatrix=impute.knn(as.matrix(allpatientsfeature_manyvariables[numericalfeatures]),k = 10, rowmax = 0.8, colmax = 0.8, maxp = 1500)$data


# scaling for COX
scalednumericalmatrix=imputednumericalmatrix
nonscalednumericalmatrix=imputednumericalmatrix
rightSkewed = c("GGT", "CK", "RBC", "Urine_ph", "AST", "ALT", "Bilirubin_Total") 
leftSkewed = c("onset_delta", "Q10", "Q10R","ALSFRS_Total") 
for(i in colnames(imputednumericalmatrix)){ 
   	if(i %in% rightSkewed){ 
     		scalednumericalmatrix[,i] = scale(log(imputednumericalmatrix[,i]+1)) 
     		nonscalednumericalmatrix[,i] = log(imputednumericalmatrix[,i]+1)
     	} else if(i %in% leftSkewed){ 
     	  scalednumericalmatrix[,i] = scale(log(-imputednumericalmatrix[,i]+max(imputednumericalmatrix[,i])+1)) 
     	  nonscalednumericalmatrix[,i] = log(-imputednumericalmatrix[,i]+max(imputednumericalmatrix[,i])+1) 
     	  
     	  } else{ 
       	  scalednumericalmatrix[,i] = scale(imputednumericalmatrix[,i]) 
       	  nonscalednumericalmatrix[,i] = imputednumericalmatrix[,i]
         	} 
   } 



# imputed_scaled is for Cox, imputed_nonscaled is for CoxBoost, nonimputed is for Randomforest.
allpatientsfeature_imputed_scaled=data.frame("SubjectID"=allpatientsfeature$SubjectID,imputedcategorical,scalednumericalmatrix)
allpatientsfeature_imputed_nonscaled=data.frame("SubjectID"=allpatientsfeature$SubjectID,imputedcategorical,nonscalednumericalmatrix)
allpatientsfeature_nonimputed=allpatientsfeature_manyvariables
 