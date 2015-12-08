rm(list = ls())
source('mySvm.R')
Data_source = 'Data/Final_Data_mean.csv'
#read data
Data = read.csv(Data_source)
#Experiemnt of number of classes
maxClass=10
# for(i in 2:maxClass)
# {
#  # Svm_Result = mySvm(Data,nClass =i,ker = "polydot",isWrite= TRUE, isTuning=TRUE,isWeight = FALSE,isTrans = FALSE,costRange = c(1,2000,50),degreeRange = c(1,4,1))  
#   Svm_Result = mySvm(Data,nClass =i,ker = "rbfdot",isWrite= TRUE, isTuning=TRUE,isWeight = FALSE,isTrans = FALSE,costRange = c(1,2000,50),degreeRange = c(1,4,1),writeFolder = 'Analysis/Tuning/')  
#  # Svm_Result = mySvm(Data,nClass =i,ker = "laplacedot",isWrite= TRUE, isTuning=TRUE,isWeight = FALSE,isTrans = FALSE,costRange = c(1,2000,50),degreeRange = c(1,4,1))  
# }

#Find the best parameter and best kernel
c = 1000



#Transition prediction unweighted data
# for(i in 2:maxClass)
# {
#   mySvm(Data,nClass =i,ker = "rbfdot",isWrite= TRUE, isTuning=FALSE,isWeight = FALSE,isTrans = TRUE,myCost = c,degreeRange = c(1,4,1),writeFolder = 'Analysis/Transition_unweighted/')  
# }
#Transition prediction with weighted data
#Transition prediction unweighted data
# for(i in 2:maxClass)
# {
#   mySvm(Data,nClass =i,ker = "rbfdot",isWrite= TRUE, isTuning=FALSE,isWeight = TRUE,isTrans = TRUE,myCost = c,degreeRange = c(1,4,1),writeFolder = 'Analysis/Transition_weighted/')  
# }

#Prediction by year

for(i in 2005:2014)
{
  for(j in 2:maxClass)
    mySvm(Data,nClass =j,ker = "rbfdot",isWrite= TRUE, isTuning=FALSE,isWeight = FALSE,isTrans = FALSE,myCost = c,degreeRange = c(1,4,1),year_test = i, writeFolder = 'Analysis/year/')  
}



#Recession accuracy


