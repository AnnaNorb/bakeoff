##########################################################################################
# SUPPORT VECTOR MACHINES PREDICTIONS
##########################################################################################

require("e1071")

##########################################################################################

for (j in 1:3) {

	nsp <- ncol(y_valid[[j]])
	nsites <- nrow(y_valid[[j]])

	ncovar<-(ncol(x_valid[[1]])-1)/2

	Xv <- x_valid[[j]][,-1]
	Xv <- Xv[,1:ncovar]

	load(file=paste(FD,set_no,"/svm_",j,"_",dataN[sz],".RData",sep=""))

	svm_notnull<-svmf[which(svmf!="NULL")]
	svm_PAs <- array(NA, dim=c(nsites,nsp,REPs))

	for (n in 1:REPs) {
		Probs <- matrix(rep(colMeans(y_train[[j]]),each=nsites),ncol=nsp)
		probs <- foreach (i=1:length(svm_notnull), .packages="e1071") %dopar% { attr(predict(svm_notnull[[i]], newdata=Xv, probability=TRUE), "probabilities")[,2] }
		Probs[,which(svmf!="NULL")] <- simplify2array(probs)
		svm_PAs[,,n] <- matrix(rbinom(Probs,1,Probs),ncol=ncol(Probs))
		rm(probs)
	}

	save(svm_PAs, file=paste(PD2,set_no,"/svm1_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(svmf)
	rm(svm_notnull)
	rm(svm_PAs)
	gc()
	
	}
##########################################################################################
