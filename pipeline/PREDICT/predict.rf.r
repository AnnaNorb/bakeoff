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

	rf1_PAs <- array(NA, dim=list(nsites,nsp,REPs))
	
	for (k in 1:nsp) {

		rff1<-NULL
		#load(file=paste(FD,set_no,"/rfs/rf_",j,"_sp",k,"_",dataN[sz],".RData",sep=""))
		load(file=paste(FD,set_no,"/rfs/rf1_",j,"_sp",k,"_",dataN[sz],".RData",sep=""))

		for (n in 1:REPs) {			

			Probs<-NULL			
			if (is.null(rff1)) {
				Probs <- matrix(rep(mean(y_train[[j]][,k]),times=nsites),ncol=1)
			} else {
				class(rff1)<-"randomForest"
				#Probs<-predict(rff1,newX=Xv,type="prob")
				Probs<-predict(rff1,newX=Xv)
			}
			Probs[which(Probs<0)]<-0
			Probs[which(Probs>1)]<-1
			#rf1_PAs[,k,n] <- matrix(rbinom(Probs,1,Probs),ncol=ncol(Probs))
			rf1_PAs[,k,n] <- matrix(rbinom(Probs,1,Probs),ncol=1)
		}
	}	

	save(rf1_PAs, file=paste(PD2,set_no,"/rf1_PAs_",j,"_",dataN[sz],".RData",sep=""))
	
	rm(rff1)
	rm(rf1_PAs)
	gc()
	
}

##########################################################################################