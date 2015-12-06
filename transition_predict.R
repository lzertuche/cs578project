set.seed(500)
Data_source = 'Data/Final_Data_mean.csv'
best_parameter = read.csv('Analysis/best_parameter.csv')
#read data
Data = read.csv(Data_source)

Data[Data[,"mktpax"]>=median(Data[,"mktpax"]),"Label"] = 1
Data[Data[,"mktpax"]<median(Data[,"mktpax"]),"Label"] = 0
temp = sort(Data[,"mktpax"])
N = nrow(Data)

# t1 = temp[round(N/3)]
# t2 = temp[round(2*N/3)]
# 
# Data[Data[,"mktpax"]<=t1,"Label"] = 0
# Data[Data[,"mktpax"]>t1 & Data[,"mktpax"]<=t2,"Label"] = 1
# Data[Data[,"mktpax"]>t2,"Label"] = 2

ntrain = round(0.6*N)
nvalid = round(0.0*N)
ntest = round(0.4*N)

train_val_sample = sample(1:N,ntrain+nvalid,replace = F)
Data_train = Data[train_val_sample[1:ntrain],]
#Data_val = Data[train_val_sample[-1:-ntrain],c(-1:-3,-5:-11)]
Data_val = Data[train_val_sample[-1:-ntrain],]
#Data_test = as.data.frame(Data[-train_val_sample,c(-1:-3,-5:-11)])
Data_test = Data[-train_val_sample,]
#model
model = Label ~ distcalc+orig_income	+ dest_income	+ orig_pop	+ dest_pop	+ Hub	+ ecoPerCapita	+ Tourist	+Industry

transition_data_test = matrix(nrow=1,ncol= ncol(Data_test))
flag = 0
for( i in 1:nrow(Data_test)){
  a = Data[,"year"] == Data_test[i,"year"]-1 & Data[,"origin"]==Data_test[i,"origin"] & Data[,"dest"] == Data_test[i,"dest"]
  
  if(sum(a)>0){
    d = Data[a,]
    if(d[,"Label"] != Data_test[i,"Label"]){
      if(flag==0){
        transition_data_test = d
        flag = 1
      }
      else
        transition_data_test = rbind(transition_data_test,d)
    }
  }
}

svm = ksvm(model,data = Data_train, kernel = "rbfdot",type = "C-svc", C=290)
pr_svm = predict(svm,transition_data_test)
Result = confusionMatrix(pr_svm,transition_data_test[,ncol(transition_data_test)])

write.csv(Result$table,'Analysis/transition_predict_median.csv')


