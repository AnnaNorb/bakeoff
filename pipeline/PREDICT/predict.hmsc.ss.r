##########################################################################################
# HMSC PREDICTIONS FORMARTTING
##########################################################################################

for (m in 1:2) {
	for (j in 1:3) {

		predFile<-file.path(PD2,set_no,"ssHMSC",paste("preds_ss_",set_no,"_hmsc", m,"_d",j,"_",dataN[sz],".csv",sep=""))
		preds<-read.csv(,header=FALSE)

		nsp <- ncol(y_valid[[j]])
		niter <- ncol(preds)/nsp

		if (m == 1) {
			nsite<-nrow(y_valid[[j]])
		} else {
			nsite<-nrow(y_valid[[j]])+nrow(y_train[[j]])
		}

		preds <- as.matrix(preds)
		ss_hmsc_PAs <- array(NA,dim=list(nsite,nsp,niter))
		ss_hmsc_PAs[,1,] <- preds[,1:niter]

		for (sp in 2:nsp) {
			ss_hmsc_PAs[,sp,] <- preds[,c(((niter*(sp-1))+1):(niter*sp))]
		}

		if (m != 1) {
			ss_hmsc_PAs<-ss_hmsc_PAs[((dim(y_train[[j]])[1])+1):nsite,,]
		}

		save(ss_hmsc_PAs, file=file.path(PD2,set_no,paste("ss_hmsc",m,"_PAs_",j,"_",dataN[sz],".RData",sep="")))

		rm(preds)
		rm(ss_hmsc_PAs)

	}
}	

##########################################################################################
	