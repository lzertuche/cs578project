rm(list = ls())
svm_poly <- read.csv('Analysis/perf_svm_poly_C_degree.csv')
svm_rbf <- read.csv('Analysis/perf_svm_rbf_C.csv')
svm_laplace <- read.csv('Analysis/perf_svm_laplace_C.csv')
svm_nb <-read.csv('Analysis/perf_naive_bayes.csv')

bestparameters = matrix(nrow=4,ncol=3)
rownames(bestparameters)<- c("poly","rbf","laplace","nb")
colnames(bestparameters) <- c("valAcc","cost","degree")
bst_poly_i = which.max(svm_poly[,"valAcc"])

bestparameters[1,"valAcc"] = svm_poly[bst_poly_i,"valAcc"]
bestparameters[1,"cost"] = svm_poly[bst_poly_i,"cost"]
bestparameters[1,"degree"] = svm_poly[bst_poly_i,"degree"]

bst_rbf_i = which.max(svm_rbf[,"valAcc"])

bestparameters[2,"valAcc"] = svm_rbf[bst_rbf_i,"valAcc"]
bestparameters[2,"cost"] = svm_rbf[bst_rbf_i,"cost"]

bst_laplace_i = which.max(svm_laplace[,"valAcc"])

bestparameters[3,"valAcc"] = svm_laplace[bst_laplace_i,"valAcc"]
bestparameters[3,"cost"] = svm_laplace[bst_laplace_i,"cost"]

bst_nb_i = which.max(svm_nb[,"testAcc"])

bestparameters[4,"valAcc"] = svm_nb[bst_rbf_i,"testAcc"]
#bestparameters[3,"cost"] = svm_poly[bst_rbf_i,"cost"]

write.csv(bestparameters,'Analysis/best_parameter.csv')
