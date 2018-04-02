##########################################################################################
# RANDOM FORESTS PREDICTIONS
##########################################################################################

require(randomForest)

##########################################################################################


for (j in 1:3) {

	nsp <- ncol(y_valid[[j]])
	nsites <- nrow(y_valid[[j]])

	ncovar<-(ncol(x_valid[[1]])-1)/2

	Xv <- x_valid[[j]][,-1]
	Xv <- Xv[,1:ncovar]

	rf_PAs <- array(NA, dim=list(nsites,nsp,REPs))
	
	for (k in 1:nsp) {

		rff<-NULL
		load(file=paste(FD,set_no,"/rfs/rf_",j,"_sp",k,"_",dataN[sz],".RData",sep=""))

		for (n in 1:REPs) {			

			Probs<-NULL			
			if (is.null(rff)) {
				Probs <- matrix(rep(mean(y_train[[j]][,k]),times=nsites),ncol=1)
			} else {
				class(rff)<-"randomForest"
				Probs<-predict(rff,newX=Xv,type="prob")
			}
			
			rf_PAs[,k,n] <- matrix(rbinom(Probs,1,Probs),ncol=ncol(Probs))
		}
	}	

	save(rf_PAs, file=paste(PD2,set_no,"/rf_PAs_",j,"_",dataN[sz],".RData",sep=""))
	
	rm(rff)
	rm(rf_PAs)
	gc()
	
}

##########################################################################################