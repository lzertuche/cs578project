rm(list = ls())
source('mySvm.R')
Data_source = 'Data/Final_Data_mean.csv'
#read data
Data = read.csv(Data_source)
Svm_Result = mySvm(Data,nClass = 6,isWrite = TRUE,isTuning=FALSE,isWeight = TRUE,isTrans = TRUE)