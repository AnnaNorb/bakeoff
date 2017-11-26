##########################################################################################
# MULTIVARIATE RANDOM FORESTS
##########################################################################################

require(MultivariateRandomForest)
source(paste(MD,"modify_mvrf.r",sep=""))

##########################################################################################

for (j in 1:3) {

	Xv <- x_valid[[j]][,-1]
	Sv <- s_valid[[j]]
	Xvs<-cbind(Xv,Sv)
	Yt <- y_train[[j]]
	Ytr<-Yt+rnorm(Yt,sd=0.001)
	
	nsp <- ncol(y_valid[[j]])
	nsites <- nrow(y_valid[[j]])

	load(file=paste(FD,set_no,"/mvrf_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/trBest_",j,"_",dataN[sz],".RData",sep=""))
		
	load(file=paste(FD,set_no,"/mvrf2_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/trBest2_",j,"_",dataN[sz],".RData",sep=""))

	mvrf_preds <- mvrf_predict(models=mvrf, trainY=Ytr, n_tree=ntrees, newX=Xv)
	mvrf2_preds <- 	mvrf_predict(models=mvrf2, trainY=Ytr, n_tree=ntrees2, newX=Xvs)

	mvrf_preds[which(mvrf_preds>1)]<-1
	mvrf2_preds[which(mvrf2_preds<0)]<-0

	mvrf_PAs <- array(NA,dim=list(nsites,nsp,REPs))
	mvrf2_PAs <- mvrf_PAs

	for (n in 1:REPs) {
		mvrf_PAs[,,n] <- matrix(rbinom(mvrf_preds,1,mvrf_preds),ncol=ncol(mvrf_preds))
		mvrf2_PAs[,,n] <- matrix(rbinom(mvrf2_preds,1,mvrf2_preds),ncol=ncol(mvrf2_preds))
		}

	save(mvrf_PAs, file=paste(PD2,set_no,"/mvrf_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(mvrf2_PAs, file=paste(PD2,set_no,"/mvrf2_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(mvrf)
	rm(mvrf2)
	rm(mvrf_preds)
	rm(mvrf2_preds)
	rm(mvrf_PAs)
	rm(mvrf2_PAs)
	gc()
	
	}
##########################################################################################
