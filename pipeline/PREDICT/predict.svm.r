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

	load(file=file.path(FD,set_no,paste("svm1_",j,"_",dataN[sz],".RData",sep="")))

	svm_notnull<-svmf1[which(svmf1!="NULL")]
	svm1_PAs <- array(NA, dim=c(nsites,nsp,REPs))

	for (n in 1:REPs) {
		Probs <- matrix(rep(colMeans(y_train[[j]]),each=nsites),ncol=nsp)
		#probs <- foreach (i=1:length(svm_notnull), .packages="e1071") %dopar% { attr(predict(svm_notnull[[i]], newdata=Xv, probability=TRUE), "probabilities")[,2] }
		probs <- foreach (i=1:length(svm_notnull), .packages="e1071") %dopar% { predict(svm_notnull[[i]], newdata=Xv) }
		probs<-simplify2array(probs)
		probs[which(probs<0)]<-0
		probs[which(probs>1)]<-1
		Probs[,which(svmf1!="NULL")] <- probs
		#svm_PAs[,,n] <- matrix(rbinom(Probs,1,Probs),ncol=ncol(Probs))
		svm1_PAs[,,n] <- matrix(rbinom(Probs,1,Probs),ncol=1)
		rm(probs)
	}

	save(svm1_PAs, file=file.path(PD2,set_no,paste("svm1_PAs_",j,"_",dataN[sz],".RData",sep="")))

	rm(svmf1)
	rm(svm_notnull)
	rm(svm1_PAs)
	gc()
	
	}
##########################################################################################
