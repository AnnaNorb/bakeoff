##########################################################################################
# RANDOM FORESTS PREDICTIONS
##########################################################################################

require(randomForest)

##########################################################################################


for (j in 1:3) {

	nsp <- ncol(y_valid[[j]])
	nsites <- nrow(y_valid[[j]])
	
	Xv <- x_valid[[j]][,-1]
	Xvs <- cbind(Xv,s_valid[[j]])

	rf_PAs <- array(NA, dim=list(nsites,nsp,REPs))
	rf2_PAs <- rf_PAs
	
	for (k in 1:nsp) {

		rff<-NULL
		load(file=paste(FD,set_no,"/rfs/rf_",j,"_sp",k,"_",dataN[sz],".RData",sep=""))
		rff2<-NULL
		load(file=paste(FD,set_no,"/rfs2/rf2_",j,"_sp",k,"_",dataN[sz],".RData",sep=""))

		for (n in 1:REPs) {
			
			Probs<-NULL
			
			if (is.null(rff)!=TRUE) {
				class(rff)<-"randomForest"
				rf_PAs[,k,n]<-rbinom(predict(rff,type="prob")[,2],1,predict(rff,type="prob")[,2])
			} else {
				Probs <- matrix(rep(mean(y_train[[j]][,k]),times=nsites),ncol=1)
				rf_PAs[,k,n] <- matrix(rbinom(Probs,1,Probs),ncol=ncol(Probs))
				}

			if (is.null(rff2)!=TRUE) {
				class(rff2)<-"randomForest"
				rf2_PAs[,k,n]<-rbinom(predict(rff2,type="prob")[,2],1,predict(rff2,type="prob")[,2])
			} else {
				Probs <- matrix(rep(mean(y_train[[j]][,k]),times=nsites),ncol=1)
				rf2_PAs[,k,n] <- matrix(rbinom(Probs,1,Probs),ncol=ncol(Probs))
				}

			}	
		}

	save(rf_PAs, file=paste(PD2,set_no,"/rf_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(rf2_PAs, file=paste(PD2,set_no,"/rf2_PAs_",j,"_",dataN[sz],".RData",sep=""))
	
	rm(rff)
	rm(rff2)
	rm(rf_PAs)
	rm(rf2_PAs)
	gc()
	
	}

##########################################################################################