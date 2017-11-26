# Multivariate Random Forests
##########################################################################################
library("pROC")
library("MultivariateRandomForest")
source(paste(MD,"mvrf/modify_mvrf.r",sep=""))

Xi_t <- X_I_train[,2:(ncovar+1)]
Yi_t <- y_I_train
Xi_v <- X_I_valid[,2:(ncovar+1)]
Yi_v <- y_I_valid

Xe_t <- X_E_train[,2:(ncovar+1)]
Ye_t <- y_E_train
Xe_v <- X_E_valid[,2:(ncovar+1)]
Ye_v <- y_E_valid

ntrees<-c(50,500,2000)
mfeat<-floor(sqrt(ncovar))
minL<-ceiling(.632*ncovar)

Yi_t<-Yi_t+rnorm(Yi_t,sd=0.001)
Ye_t<-Ye_t+rnorm(Ye_t,sd=0.001)

predtrys_I<-list()
predtrys_E<-list()

# interpolation
sT<-Sys.time()
for (tr in 1:length(ntrees)) {
predtrys_I[[tr]] <- build_forest_predict(trainX=Xi_t, trainY=Yi_t, n_tree=ntrees[tr], m_feature=mfeat, min_leaf=minL, testX=Xi_t)
}
tmp <- lapply(lapply(predtrys_I, as.vector), auc, response=as.vector(y_I_train))
tr<-which(unlist(tmp)==max(unlist(tmp)))[1]
mvrf_I <- build_forest_predict(trainX=Xi_t, trainY=Yi_t, n_tree=ntrees[tr], m_feature=mfeat, min_leaf=minL, testX=Xi_v)
eT<-Sys.time()
comTimes$MVRF<-eT-sT

save(mvrf_I, file=paste(FD,set_no,"/mvrf_I.r",sep=""))

# extrapolation
for (tr in 1:length(ntrees)) {
predtrys_E[[tr]] <- build_forest_predict(trainX=Xe_t, trainY=Ye_t, n_tree=ntrees[tr], m_feature=mfeat, min_leaf=minL, testX=Xe_t)
}
tmp <- lapply(lapply(predtrys_E, as.vector), auc, response=as.vector(y_E_train))
tr <- which(unlist(tmp)==max(unlist(tmp)))[1]
mvrf_E <-  build_forest_predict(trainX=Xe_t, trainY=Ye_t, n_tree=ntrees[tr], m_feature=mfeat, min_leaf=minL, testX=Xe_v)

save(mvrf_E, file=paste(FD,set_no,"/mvrf_E.r",sep=""))

# 
# trainY=matrix(runif(50*5),50,5) 
# trainY<-round(trainY)
# n_tree=2
# m_feature=5
# min_leaf=5
# testX=matrix(runif(10*100),10,100) 
# #Prediction size is 10 x 5, where 10 is the number 
# #of testing samples and 5 is the number of output features
# Prediction1<-build_forest_predict(trainX, trainY, n_tree, m_feature, min_leaf, testX=trainX)