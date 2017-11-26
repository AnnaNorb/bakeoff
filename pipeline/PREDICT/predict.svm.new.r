##########################################################################################
# SUPPORT VECTOR MACHINES PREDICTIONS
##########################################################################################

library("e1071")

##########################################################################################

for (j in 1:3) {

	nsp <- length(DD_v[[j]])
	Xv <- x_valid[[j]][,-1]
	Xvs <- cbind(Xv,s_valid[[j]])
	
	nsites<-dim(x_valid[[j]])[1]
	nsp<-dim(y_valid[[j]])[2]

	load(file=paste(FD,set_no,"/svm_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/svm2_",j,"_",dataN[sz],".RData",sep=""))

	Probs <- matrix(rep(colMeans(y_train[[j]]),each=nsites),ncol=nsp)
	Probs2 <- Probs
	
	svm_notnull<-svmf[which(svmf!="NULL")]
	svm_PAs <- array(NA, dim=c(nsites,nsp,REPs))

	svm2_notnull<-svmf2[which(svmf2!="NULL")]
	svm2_PAs <- array(NA, dim=c(nsites,nsp,REPs))

	for (k in 1:REPs) {
		probs <- foreach (i=1:length(svm_notnull), .packages="e1071") %dopar% { attr(predict(svm_notnull[[i]], newdata=Xvs, probability=TRUE), "probabilities")[,2] }
		Probs[,which(svmf!="NULL")] <- simplify2array(probs)
		svm_PAs[,,k] <- matrix(rbinom(Probs,1,Probs),ncol=ncol(Probs))
		rm(probs)
		probs <- foreach (i=1:length(svm2_notnull), .packages="e1071") %dopar% { attr(predict(svm2_notnull[[i]], newdata=Xvs, probability=TRUE), "probabilities")[,2] }
		Probs2[,which(svmf2!="NULL")] <- simplify2array(probs)
		svm2_PAs[,,k] <- matrix(rbinom(Probs2,1,Probs2),ncol=ncol(Probs2))
		rm(probs)
		}


	save(svm_PAs, file=paste(PD2,set_no,"/svm_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(svm2_PAs, file=paste(PD2,set_no,"/svm2_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(svmf)
	rm(svmf2)
	rm(svm_notnull)
	rm(svm2_notnull)
	rm(svmf)
	rm(svmf2)
	rm(svm_PAs)
	rm(svm2_PAs)
	
	gc()
	
	}
##########################################################################################
