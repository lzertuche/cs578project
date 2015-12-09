#Include this file in the desired code by source('mySvm.R')
#Data<- The data set
#nClass = number of class: default = 2
#ker = kernel used: "rbfdot","laplacedot","polydot". It's important to use this name.
#isTuning= is tuning is set
#pertrain= percentage of training
#...
#...
#myCOst= default cost param
#...


##Output
#It return three object as a list. YOu have to acces it like
#
#t = mySvm()
#t[1] is the performance: useful when tuning is used
#t[2] is the svm model: useful when not tuning used. it is the training model outcome
#t[3] is the detailed result: also useful when tuning is not used. 
#

weightedAccuracy<-function(pr,ac,nClass)
{
  print(length(pr))
  print(length(ac))
  pr_weight = 1-abs(pr-ac)/nClass
  acc = sum(pr_weight)/length(pr_weight)
}

mySvm<-function(Data,nClass=2,ker="rbfdot",isTuning=TRUE,perTrain=0.6,perVal=0.2,perTest=0.2,myCost=600,myDegree=2,costRange=c(1,1000,50),degreeRange=c(1,4,1),isWeight=FALSE,isTrans=FALSE,isWrite=TRUE,writeFolder='Analysis/',year_test=NULL,isWeightAcc=FALSE)
{
  print(nClass)
  library("dplyr")
  library(neuralnet)
  library(kernlab)
  library(e1071)
  library(caret)
  #library(dataframes2xls)
  #require(xlsx)
  
  #data source
  set.seed(500)
  #Data_source = 'Data/Final_Data_mean.csv'
  #read data
  #Data = read.csv(Data_source)
  
  #data dichotomies mean/median, You can use mean instead of median
  # Data[Data[,"mktpax"]>=median(Data[,"mktpax"]),"Label"] = 1
  # Data[Data[,"mktpax"]<median(Data[,"mktpax"]),"Label"] = 0
  
  Data$Label <- ntile(Data$mktpax,nClass)-1
  
  
  #write.csv(Data,'Data/Final_Data_weighted.csv')
  N = nrow(Data)
  
  ntrain = round(perTrain*N)
  nvalid = round(perVal*N)
  ntest = round(perTest*N)
  
  
  
  
  train_val_sample = sample(1:N,ntrain+nvalid,replace = F)
  Data_train = Data[train_val_sample[1:ntrain],]
  #Data_val = Data[train_val_sample[-1:-ntrain],c(-1:-3,-5:-11)]
  Data_val = Data[train_val_sample[-1:-ntrain],]
  #Data_test = as.data.frame(Data[-train_val_sample,c(-1:-3,-5:-11)])
  Data_test = Data[-train_val_sample,]
  
  if(is.null(year_test)==FALSE)
  {
    a = Data_test[,"year"]==year_test
    Data_test = Data_test[a,]
  }
  else if(isTrans==TRUE)
  {
    transition_data_test = matrix(nrow=1,ncol= ncol(Data_test))
    flag = 0
    for( i in 1:nrow(Data_test)){
      a = Data[,"year"] == Data_test[i,"year"]-1 & Data[,"origin"]==Data_test[i,"origin"] & Data[,"dest"] == Data_test[i,"dest"]
      
      if(sum(a)>0){
        d = Data[a,]
        if(d[1,"Label"] != Data_test[i,"Label"]){
          if(flag==0){
            transition_data_test = d
            flag = 1
          }
          else
            transition_data_test = rbind(transition_data_test,d)
        }
      }
    }
    Data_test = transition_data_test
    
  }
  #replicated the data 
  if(isWeight==TRUE){
    for( i in 1:ntrain){
      a = Data[,"year"] == Data_train[i,"year"]-1 & Data[,"origin"]==Data_train[i,"origin"] & Data[,"dest"] == Data_train[i,"dest"]
      
      if(sum(a)>0){
        d = Data[a,]
        if(d[1,"Label"] != Data_train[i,"Label"]){
          #       if(flag==0){
          #         transition_data_test = d
          #         flag = 1
          #       }
          #       else
          #         transition_data_test = rbind(transition_data_test,d)
          Data_train = rbind(Data_train,Data_train[i,],Data_train[i,])
        }
      }
    }
  }
  
  
  
  #model
  model = Label ~ distcalc+orig_income	+ dest_income	+ orig_pop	+ dest_pop	+ Hub	+ ecoPerCapita	+ Tourist	+Industry
  grid <- NULL
  if(isTuning==FALSE)
  {
    
    Performance = matrix(nrow = 1,ncol = 3)
    colnames(Performance) <- c("trainAcc","valAcc","testAcc")
    if(ker=="polydot")
    {
      svm = ksvm(model,data = Data_train, kernel = "polydot",type = "C-svc", C=myCost,kpar = list(degree = myDegree))
    }
    else
    {
      svm = ksvm(model,data = Data_train, kernel = ker,type = "C-svc", C=myCost)
    }
    pr_svm = predict(svm,Data_val[,-ncol(Data_val)])
    print(svm)
    #print(acc)
    
    
    
    Result = confusionMatrix(pr_svm,Data_val[,ncol(Data_val)])
    
    if(isWeightAcc==TRUE) acc <- weightedAccuracy(pr_svm,Data_val[,ncol(Data_val)],nClass)
    else acc <- Result$overall[1]
    
    Performance[1,"trainAcc"] = 1-error(svm)
    Performance[1,"valAcc"] = acc
    
    pr_svm_test = predict(svm,Data_test[,-ncol(Data_test)])
    Result = confusionMatrix(pr_svm_test,Data_test[,ncol(Data_test)])
    
    if(isWeightAcc==TRUE) acc <- weightedAccuracy(pr_svm_test,Data_test[,ncol(Data_test)],nClass)
    else acc <- Result$overall[1]
    
    #test performance
    Performance[1,"testAcc"] = acc
  
  }
  else
  {
    if(ker=="polydot"){
      
      grid <-expand.grid(cost = seq(costRange[1],costRange[2],costRange[3]),degree=seq(degreeRange[1],degreeRange[2],degreeRange[3]))
      #print(grid)
      Performance = matrix(nrow = nrow(grid),ncol = 3)
      colnames(Performance) <- c("trainAcc","valAcc","testAcc")
      for(i in 1:nrow(grid)){
        #svm = ksvm(model,data = Data_train, kernel = "rbfdot",type = "C-svc", C=grid$cost[i])
        svm = ksvm(model,data = Data_train, kernel = "polydot",type = "C-svc", C=grid$cost[i],kpar = list(degree = grid$degree[i]))
        pr_svm = predict(svm,Data_val[,-ncol(Data_val)])
        # acc = sum(cbind(pr_svm) ==  (cbind(Data_test[,ncol(Data_test)])))/ nrow(Data_test)
        #acc = sum(cbind(pr_svm) ==  (cbind(Data_val[,ncol(Data_val)])))/ nrow(Data_val)
        print(svm)
        #print(acc)
        
        Result = confusionMatrix(pr_svm,Data_val[,ncol(Data_val)])
        
        if(isWeightAcc==TRUE) acc <- weightedAccuracy(pr_svm,Data_val[,ncol(Data_val)],nClass)
        else acc <- Result$overall[1]
        
        Performance[i,"trainAcc"] = 1-error(svm)
        Performance[i,"valAcc"] = acc
        #   tp = Result$table[2,2]
        #   tn = Result$table[1,1]
        #   fp = Result$table[2,1]
        #   fn = Result$table[1,2]
        #   Performance[i,"valPrecision"] = tp/(tp+fp)
        #   Performance[i,"valRecall"] = tp/(tp+fn)
        #   Performance[i,"valF1_score"] = 2*Performance[i,"valPrecision"]*Performance[i,"valRecall"]/(Performance[i,"valPrecision"]+Performance[i,"valRecall"])
        
        #test
        pr_svm_test = predict(svm,Data_test[,-ncol(Data_test)])
        Result = confusionMatrix(pr_svm_test,Data_test[,ncol(Data_test)])
        
        if(isWeightAcc==TRUE) acc <- weightedAccuracy(pr_svm_test,Data_test[,ncol(Data_test)],nClass)
        else acc <- Result$overall[1]
        
        #test performance
        Performance[i,"testAcc"] = acc
        #   tp = Result$table[2,2]
        #   tn = Result$table[1,1]
        #   fp = Result$table[2,1]
        #   fn = Result$table[1,2]
        #   Performance[i,"testPrecision"] = tp/(tp+fp)
        #   Performance[i,"testRecall"] = tp/(tp+fn)
        #   Performance[i,"testF1_score"] = 2*Performance[i,"testPrecision"]*Performance[i,"testRecall"]/(Performance[i,"testPrecision"]+Performance[i,"testRecall"])
        
        
        
      }
    }
    else
    {
      #Tuning cost parameter with rbf and laplace dotdot
      grid <-expand.grid(cost = seq(costRange[1],costRange[2],costRange[3]))
      Performance = matrix(nrow = nrow(grid),ncol = 3)
      colnames(Performance) <- c("trainAcc","valAcc","testAcc")
      for(i in 1:nrow(grid)){
        svm = ksvm(model,data = Data_train, kernel = ker,type = "C-svc", C=grid$cost[i])
        
        pr_svm = predict(svm,Data_val[,-ncol(Data_val)])
        # acc = sum(cbind(pr_svm) ==  (cbind(Data_test[,ncol(Data_test)])))/ nrow(Data_test)
        #acc = sum(cbind(pr_svm) ==  (cbind(Data_val[,ncol(Data_val)])))/ nrow(Data_val)
        print(svm)
        #print(acc)
        Result = confusionMatrix(pr_svm,Data_val[,ncol(Data_val)])
        
        if(isWeightAcc==TRUE) acc <- weightedAccuracy(pr_svm,Data_val[,ncol(Data_val)],nClass)
        else acc <- Result$overall[1]
        
        Performance[i,"trainAcc"] = 1-error(svm)
        Performance[i,"valAcc"] = acc
        #   tp = Result$table[2,2]
        #   tn = Result$table[1,1]
        #   fp = Result$table[2,1]
        #   fn = Result$table[1,2]
        #   Performance[i,"valPrecision"] = tp/(tp+fp)
        #   Performance[i,"valRecall"] = tp/(tp+fn)
        #   Performance[i,"valF1_score"] = 2*Performance[i,"valPrecision"]*Performance[i,"valRecall"]/(Performance[i,"valPrecision"]+Performance[i,"valRecall"])
        
        
        #test
        pr_svm_test = predict(svm,Data_test[,-ncol(Data_test)])
        Result = confusionMatrix(pr_svm_test,Data_test[,ncol(Data_test)])
        
        if(isWeightAcc==TRUE) acc <- weightedAccuracy(pr_svm_test,Data_test[,ncol(Data_test)],nClass)
        else acc <- Result$overall[1]
        
        #test performance
        Performance[i,"testAcc"] = acc
        #   tp = Result$table[2,2]
        #   tn = Result$table[1,1]
        #   fp = Result$table[2,1]
        #   fn = Result$table[1,2]
        #   Performance[i,"testPrecision"] = tp/(tp+fp)
        #   Performance[i,"testRecall"] = tp/(tp+fn)
        #   Performance[i,"testF1_score"] = 2*Performance[i,"testPrecision"]*Performance[i,"testRecall"]/(Performance[i,"testPrecision"]+Performance[i,"testRecall"])
        #   
        
      }
      
      
    }
    
  }
  print(grid)
  if(isWrite==TRUE)
  {
    filename = paste(writeFolder,'svm_perf_',nClass,sep="")
    if(is.null(year_test)==FALSE) filename = paste(filename,'_',year_test,sep="")
    if(isTrans==TRUE)
    { 
      filename= paste(filename,'_','trans',sep="")
    }
    if(isWeight==TRUE) filename= paste(filename,'_','weight',sep="")
    
    if(isTuning==TRUE) filename= paste(filename,'_','tune',sep="")
    
    
#     if(isTrans==TRUE)
#     { 
#       if(isWeight==TRUE) filename= paste(writeFolder,'svm_perf_',ker,'_',nClass,'_','trans_weight',year_test,'.csv',sep="")
#       else filename= paste(writeFolder,'svm_perf_',ker,'_',nClass,'_','trans','.csv',year_test,sep="")
#     }
#     else
#     {
#       if(isWeight==TRUE) filename= paste(writeFolder,'svm_perf_',ker,'_',nClass,'_','weight',year_test,'.csv',sep="")
#       else filename= paste(writeFolder,'svm_perf_',ker,'_',nClass,year_test,'.csv',sep="")
#     }
#     if (isTuning==TRUE)
#     {
#       file
#     }
    print(filename)
    if(isTuning==TRUE) 
    {
      df = cbind(grid,Performance)
      write.csv(df,paste(filename,".csv",sep=""))
    }
    else 
    {
      write.csv(Performance,paste(filename,".csv",sep=""))
      write.csv(Result$table,paste(filename,'_table',".csv",sep=""))
    }
  }
  return(list(Performance,svm,Result))  
  
}
