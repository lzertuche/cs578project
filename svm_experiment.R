rm(list = ls())
source('mySvm.R')
Data_source = 'Data/Final_Data_mean.csv'
#read data
Data = read.csv(Data_source)
#Experiemnt of number of classes
maxClass=10
for(i in 2:maxClass)
{
  Svm_Result = mySvm(Data,nClass =i,ker = "polydot",isWrite= TRUE, isTuning=TRUE,isWeight = FALSE,isTrans = FALSE,costRange = c(1,2000,50),degreeRange = c(1,4,1))  
  Svm_Result = mySvm(Data,nClass =i,ker = "rbfydot",isWrite= TRUE, isTuning=TRUE,isWeight = FALSE,isTrans = FALSE,costRange = c(1,2000,50),degreeRange = c(1,4,1))  
  Svm_Result = mySvm(Data,nClass =i,ker = "laplacedot",isWrite= TRUE, isTuning=TRUE,isWeight = FALSE,isTrans = FALSE,costRange = c(1,2000,50),degreeRange = c(1,4,1))  
}

#Find the best parameter and best kernel



#Transition prediction without weighted data

#Transition prediction with weighted data


#Recession accuracy


