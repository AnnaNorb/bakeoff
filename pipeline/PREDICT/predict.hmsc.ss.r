##########################################################################################
# HIERSRCHICAL MODELLING OF SPECIES COMMUNITIES PREDICTIONS FORMARTTING
##########################################################################################

for (m in 1:4) {
	for (j in 1:3) {

		preds<-read.csv(paste(WD2,set_no, "/ss/preds_ss_m", m,"_d",j,"_",dataN[sz],".csv", sep=""),header=FALSE)

		nsp <- ncol(y_valid[[j]])
		niter <- ncol(preds)/nsp

		if (m==4) {
			nsite <- dim(y_valid[[j]])[1]+dim(y_valid[[j]])[1]
		} else {
			nsite <- dim(y_valid[[j]])[1]
			}

		preds <- as.matrix(preds)
		ss_hmsc_PAs <- array(NA,dim=list(nsite,nsp,niter))
		ss_hmsc_PAs[,1,] <- preds[,1:niter]

		for (sp in 2:nsp) {
			ss_hmsc_PAs[,sp,] <- preds[,c(((niter*(sp-1))+1):(niter*sp))]
		}

		if (m==4) {
			ss_hmsc_PAs<-ss_hmsc_PAs[((dim(y_train[[j]])[1])+1):nsite,,]
		}

		save(ss_hmsc_PAs, file=paste(PD2,set_no,"/ss_hmsc",m,"_PAs_",j,"_",dataN[sz],".RData",sep=""))

		rm(preds)
		rm(ss_hmsc_PAs)

	}
}	

##########################################################################################
	