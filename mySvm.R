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
mySvm<-function(Data,nClass=2,ker="rbfdot",isTuning=TRUE,perTrain=0.6,perVal=0.2,perTest=0.2,myCost=600,myDegree=2,costRange=c(1,1000,50),degreeRange=c(1,4,1),isWeight=FALSE,isTrans=FALSE,isWrite=TRUE,writeFolder='Analysis/')
{
  print(nClass)
  require("caret")
  library("dplyr")
  library(neuralnet)
  library(kernlab)
  library(e1071)
  library(caret)
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
  
  if(isTrans==TRUE)
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
    Performance[1,"trainAcc"] = 1-error(svm)
    Performance[1,"valAcc"] = Result$overall[1]
    
    pr_svm_test = predict(svm,Data_test[,-ncol(Data_test)])
    Result = confusionMatrix(pr_svm_test,Data_test[,ncol(Data_test)])
    
    #test performance
    Performance[1,"testAcc"] = Result$overall[1]
  
  }
  else
  {
    if(ker=="polydot"){
      grid <-expand.grid(cost = seq(costRange),degree=seq(degreeRange))
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
        Performance[i,"trainAcc"] = 1-error(svm)
        Performance[i,"valAcc"] = Result$overall[1]
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
        
        #test performance
        Performance[i,"testAcc"] = Result$overall[1]
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
      grid <-expand.grid(cost = seq(1,5000,50))
      Performance = matrix(nrow = nrow(grid),ncol = 9)
      colnames(Performance) <- c("trainAcc","valAcc","valPrecision","valRecall","valF1_score","testAcc","testPrecision","testRecall","testF1_score")
      for(i in 1:nrow(grid)){
        svm = ksvm(model,data = Data_train, kernel = ker,type = "C-svc", C=grid$cost[i])
        
        pr_svm = predict(svm,Data_val[,-ncol(Data_val)])
        # acc = sum(cbind(pr_svm) ==  (cbind(Data_test[,ncol(Data_test)])))/ nrow(Data_test)
        #acc = sum(cbind(pr_svm) ==  (cbind(Data_val[,ncol(Data_val)])))/ nrow(Data_val)
        print(svm)
        #print(acc)
        Result = confusionMatrix(pr_svm,Data_val[,ncol(Data_val)])
        Performance[i,"trainAcc"] = 1-error(svm)
        Performance[i,"valAcc"] = Result$overall[1]
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
        
        #test performance
        Performance[i,"testAcc"] = Result$overall[1]
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
  if(isWrite==TRUE)
  {
    if(isTrans==TRUE)
    { 
      if(isWeight==TRUE) filename= paste(writeFolder,'svm_perf_',ker,'_',nClass,'_','trans_weight','.csv',sep="")
      else filename= paste(writeFolder,'svm_perf_',ker,'_',nClass,'_','trans','.csv',sep="")
    }
    else
    {
      if(isWeight==TRUE) filename= paste(writeFolder,'svm_perf_',ker,'_',nClass,'_','weight','.csv',sep="")
      else filename= paste(writeFolder,'svm_perf_',ker,'_',nClass,'.csv',sep="")
    }
    print(filename)
    if(isTuning==TRUE) write.csv(cbind(grid,Performance),filename)
    else write.csv(Performance,filename)
  }
  return(list(Performance,svm,Result))  
  
}