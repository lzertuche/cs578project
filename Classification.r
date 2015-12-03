rm(list = ls())
require("caret")
#data source
set.seed(500)
Data_source = '/Data/Final_Data_mean.csv'
#read data
Data = read.csv(Data_source)
N = nrow(Data)
ntrain = round(0.6*N)
nvalid = round(0.2*N)
ntest = round(0.2*N)

train_val_sample = sample(1:N,ntrain+nvalid,replace = F)
Data_train = Data[train_val_sample[1:ntrain],]
#Data_val = Data[train_val_sample[-1:-ntrain],c(-1:-3,-5:-11)]
Data_val = Data[train_val_sample[-1:-ntrain],]
#Data_test = as.data.frame(Data[-train_val_sample,c(-1:-3,-5:-11)])
Data_test = Data[-train_val_sample,]
#model
model = Label ~ distcalc+orig_income	+ dest_income	+ orig_pop	+ dest_pop	+ Hub	+ ecoPerCapita	+ Tourist	+Industry
library(neuralnet)
library(kernlab)
library(e1071)
library(caret)

#grid <-expand.grid(cost = 2^(1:15))



# nn <- neuralnet( model, data = Data[1:(nrow(Data)-100),], hidden = c(6,4,2), err.fct = "ce", linear.output = FALSE)
# plot(nn)
# print(nn$result.matrix)




# pr = compute(nn,Data_test)
# print(cbind(pr$net.result,Data[-1:-nrow(Data)-100,ncol(Data)]))
# 
# acc = sum((pr$net.result>0.5) ==  (cbind(Data[-1:-nrow(Data)-100,ncol(Data)]==1)))/ nrow(Data_test)
# print(acc)


#Tuning cost parameter with poly dot
grid <-expand.grid(cost = seq(1,500,10),degree=1:4)
Performance = matrix(nrow = nrow(grid),ncol = 9)
colnames(Performance) <- c("trainAcc","valAcc","valPrecision","valRecall","valF1_score","testAcc","testPrecision","testRecall","testF1_score")
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
  tp = Result$table[2,2]
  tn = Result$table[1,1]
  fp = Result$table[2,1]
  fn = Result$table[1,2]
  Performance[i,"valPrecision"] = tp/(tp+fp)
  Performance[i,"valRecall"] = tp/(tp+fn)
  Performance[i,"valF1_score"] = 2*Performance[i,"valPrecision"]*Performance[i,"valRecall"]/(Performance[i,"valPrecision"]+Performance[i,"valRecall"])
  
  #test
  pr_svm_test = predict(svm,Data_test[,-ncol(Data_test)])
  Result = confusionMatrix(pr_svm_test,Data_test[,ncol(Data_test)])
  
  #test performance
  Performance[i,"testAcc"] = Result$overall[1]
  tp = Result$table[2,2]
  tn = Result$table[1,1]
  fp = Result$table[2,1]
  fn = Result$table[1,2]
  Performance[i,"testPrecision"] = tp/(tp+fp)
  Performance[i,"testRecall"] = tp/(tp+fn)
  Performance[i,"testF1_score"] = 2*Performance[i,"testPrecision"]*Performance[i,"testRecall"]/(Performance[i,"testPrecision"]+Performance[i,"testRecall"])
  
  
  
}
write.csv(cbind(grid,Performance),'Analysis/perf_svm_poly_C_degree.csv')






#Tuning cost parameter with rbf dot
grid <-expand.grid(cost = seq(1,500,10))
Performance = matrix(nrow = nrow(grid),ncol = 9)
colnames(Performance) <- c("trainAcc","valAcc","valPrecision","valRecall","valF1_score","testAcc","testPrecision","testRecall","testF1_score")
for(i in 1:nrow(grid)){
  svm = ksvm(model,data = Data_train, kernel = "rbfdot",type = "C-svc", C=grid$cost[i])

  pr_svm = predict(svm,Data_val[,-ncol(Data_val)])
  # acc = sum(cbind(pr_svm) ==  (cbind(Data_test[,ncol(Data_test)])))/ nrow(Data_test)
  #acc = sum(cbind(pr_svm) ==  (cbind(Data_val[,ncol(Data_val)])))/ nrow(Data_val)
  print(svm)
  #print(acc)
  Result = confusionMatrix(pr_svm,Data_val[,ncol(Data_val)])
  Performance[i,"trainAcc"] = 1-error(svm)
  Performance[i,"valAcc"] = Result$overall[1]
  tp = Result$table[2,2]
  tn = Result$table[1,1]
  fp = Result$table[2,1]
  fn = Result$table[1,2]
  Performance[i,"valPrecision"] = tp/(tp+fp)
  Performance[i,"valRecall"] = tp/(tp+fn)
  Performance[i,"valF1_score"] = 2*Performance[i,"valPrecision"]*Performance[i,"valRecall"]/(Performance[i,"valPrecision"]+Performance[i,"valRecall"])

  
  #test
  pr_svm_test = predict(svm,Data_test[,-ncol(Data_test)])
  Result = confusionMatrix(pr_svm_test,Data_test[,ncol(Data_test)])
  
  #test performance
  Performance[i,"testAcc"] = Result$overall[1]
  tp = Result$table[2,2]
  tn = Result$table[1,1]
  fp = Result$table[2,1]
  fn = Result$table[1,2]
  Performance[i,"testPrecision"] = tp/(tp+fp)
  Performance[i,"testRecall"] = tp/(tp+fn)
  Performance[i,"testF1_score"] = 2*Performance[i,"testPrecision"]*Performance[i,"testRecall"]/(Performance[i,"testPrecision"]+Performance[i,"testRecall"])
  

}
write.csv(cbind(grid,Performance),'Analysis/perf_svm_rbf_C.csv')


#Tuning cost parameter with laplace dot
grid <-expand.grid(cost = seq(1,500,10))
Performance = matrix(nrow = nrow(grid),ncol = 9)
colnames(Performance) <- c("trainAcc","valAcc","valPrecision","valRecall","valF1_score","testAcc","testPrecision","testRecall","testF1_score")
for(i in 1:nrow(grid)){
  svm = ksvm(model,data = Data_train, kernel = "laplacedot",type = "C-svc", C=grid$cost[i])
  
  pr_svm = predict(svm,Data_val[,-ncol(Data_val)])
  # acc = sum(cbind(pr_svm) ==  (cbind(Data_test[,ncol(Data_test)])))/ nrow(Data_test)
  #acc = sum(cbind(pr_svm) ==  (cbind(Data_val[,ncol(Data_val)])))/ nrow(Data_val)
  print(svm)
  #print(acc)
  Result = confusionMatrix(pr_svm,Data_val[,ncol(Data_val)])
  Performance[i,"trainAcc"] = 1-error(svm)
  Performance[i,"valAcc"] = Result$overall[1]
  tp = Result$table[2,2]
  tn = Result$table[1,1]
  fp = Result$table[2,1]
  fn = Result$table[1,2]
  Performance[i,"valPrecision"] = tp/(tp+fp)
  Performance[i,"valRecall"] = tp/(tp+fn)
  Performance[i,"valF1_score"] = 2*Performance[i,"valPrecision"]*Performance[i,"valRecall"]/(Performance[i,"valPrecision"]+Performance[i,"valRecall"])

  #test
  pr_svm_test = predict(svm,Data_test[,-ncol(Data_test)])
  Result = confusionMatrix(pr_svm_test,Data_test[,ncol(Data_test)])
  
  #test performance
  Performance[i,"testAcc"] = Result$overall[1]
  tp = Result$table[2,2]
  tn = Result$table[1,1]
  fp = Result$table[2,1]
  fn = Result$table[1,2]
  Performance[i,"testPrecision"] = tp/(tp+fp)
  Performance[i,"testRecall"] = tp/(tp+fn)
  Performance[i,"testF1_score"] = 2*Performance[i,"testPrecision"]*Performance[i,"testRecall"]/(Performance[i,"testPrecision"]+Performance[i,"testRecall"])
  
  
}
write.csv(cbind(grid,Performance),'Analysis/perf_svm_laplace_C.csv')


##Generative model: Naive Bayes
##Whole train and validation data

Performance = matrix(nrow = 1,ncol = 5)
colnames(Performance) <- c("trainAcc","testAcc","testPrecision","testRecall","testF1_score")
nb = naiveBayes(model,data=Data[train_val_sample,],type="class")

#train accuracy
pr_nb = predict(nb,Data_train[,-ncol(Data_train)],type="raw")
pr_nb = apply(pr_nb,1,which.max)-1
Result = confusionMatrix(pr_nb,Data_train[,ncol(Data_train)])
Performance[1,"trainAcc"] = Result$overall[1]


#test accuracy
pr_nb = predict(nb,Data_test[,-ncol(Data_test)],type="raw")
pr_nb = apply(pr_nb,1,which.max)-1
Result = confusionMatrix(pr_nb,Data_test[,ncol(Data_test)])


Performance[1,"testAcc"] = Result$overall[1]
tp = Result$table[2,2]
tn = Result$table[1,1]
fp = Result$table[2,1]
fn = Result$table[1,2]
Performance[1,"testPrecision"]  = tp/(tp+fp)
Performance[1,"testRecall"]  = tp/(tp+fn)
Performance[1,"testF1_score"]  = 2*Performance[1,"testPrecision"]*Performance[1,"testRecall"]/(Performance[1,"testPrecision"]+Performance[1,"testRecall"])



write.csv(Performance,'Analysis/perf_naive_bayes.csv')

