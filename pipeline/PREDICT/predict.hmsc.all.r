##########################################################################################
# HMSC PREDICTIONS FORMARTTING
##########################################################################################

for (m in 1:3) {
	for (j in 1:3) {

		predsFile<-file.path(PD2,set_no, paste("preds_",set_no,"_hmsc", m,"_d",j,"_",dataN[sz],".csv",sep=""))
		preds<-read.csv(predsFile,header=FALSE)

		nsp <- ncol(y_valid[[j]])
		niter <- ncol(preds)/nsp

		if (m==1) {
			nsite<-nrow(y_valid[[j]])
		} else {
			nsite<-nrow(y_valid[[j]])+nrow(y_train[[j]])
		}

		hmsc_PAs <- array(as.matrix(preds), dim=list(nsite,nsp,niter))

		if (m != 1) {
			hmsc_PAs<-hmsc_PAs[(nrow(y_train[[j]])+1):nsite,,]
		}

		save(hmsc_PAs, file=file.path(PD2,set_no,paste("hmsc",m,"_PAs_",j,"_",dataN[sz],".RData",sep="")))

		rm(preds)
		rm(hmsc_PAs)
		gc()
		}
	}

##########################################################################################
