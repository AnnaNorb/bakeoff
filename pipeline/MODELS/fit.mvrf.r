##########################################################################################
# MULTIVARIATE RANDOM FORESTS
##########################################################################################

require(pROC)
require(MultivariateRandomForest)
source(paste(MD,"modify_mvrf.r",sep=""))

##########################################################################################

for (j in 1:3) {

	Xt <- x_train[[j]][,-1]
	Xv <- x_valid[[j]][,-1]
	St <- s_train[[j]]
	Sv <- s_valid[[j]]
	Xts<-cbind(Xt,St)
	Xvs<-cbind(Xv,Sv)
	Yt <- y_train[[j]]
	Ytr<-Yt+rnorm(Yt,sd=0.001)

	ncovar<-ncol(Xt)
	ncovar2<-ncol(Xts)

	ntrees<-c(500,2000,4000)
	mfeat<-floor(sqrt(ncovar))
	mfeat2<-floor(sqrt(ncovar2))
	minL<-ceiling(.632*ncovar)
	minL2<-ceiling(.632*ncovar2)

	predtrys<-list()

	if (j==1) { sT<-Sys.time() }
	for (tr in 1:length(ntrees)) {
		predtrys[[tr]] <- build_forest_predict(trainX=Xt, trainY=Ytr, n_tree=ntrees[tr], m_feature=mfeat, min_leaf=minL, testX=Xt)
	}	

	tmp <- lapply(lapply(predtrys, as.vector), auc, response=as.vector(Yt))
	trBest<-which(unlist(tmp)==max(unlist(tmp)))[1]
	mvrf <- build_forest_predict2(trainX=Xt, trainY=Ytr, n_tree=ntrees[trBest], m_feature=mfeat, min_leaf=minL, testX=Xv)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}

	ntrees<-ntrees[trBest]
	save(ntrees, file=paste(FD,set_no,"/trBest_",j,"_",dataN[sz],".RData",sep=""))
	save(mvrf, file=paste(FD,set_no,"/mvrf_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_MVRF1_",dataN[sz],".RData",sep=""))
	}

	ncovar<-ncol(Xts)
	predtrys<-list()

	if (j==1) { sT<-Sys.time() }
	for (tr in 1:length(ntrees)) {
		predtrys[[tr]] <- build_forest_predict(trainX=Xts, trainY=Ytr, n_tree=ntrees[tr], m_feature=mfeat2, min_leaf=minL2, testX=Xts)
	}	

	tmp <- lapply(lapply(predtrys, as.vector), auc, response=as.vector(Yt))
	trBest<-which(unlist(tmp)==max(unlist(tmp)))[1]
	mvrf2 <- build_forest_predict2(trainX=Xts, trainY=Ytr, n_tree=ntrees[trBest], m_feature=mfeat2, min_leaf=minL2, testX=Xvs)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}

	ntrees2<-ntrees[trBest]
	save(ntrees2, file=paste(FD,set_no,"/trBest2_",j,"_",dataN[sz],".RData",sep=""))
	save(mvrf2, file=paste(FD,set_no,"/mvrf2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_MVRF2_",dataN[sz],".RData",sep=""))
	}
}


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