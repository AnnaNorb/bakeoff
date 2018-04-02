##########################################################################################
# BayesComm PREDICTIONS
##########################################################################################

require(BayesComm)

##########################################################################################

for (j in 1:3) {	
	load(file=paste(FD,set_no,"/no0sp_BC_",j,"_",dataN[sz],".RData",sep=""))
		
	for (m in 1:2) {
		load(file=paste(FD,set_no,"/bc",m,"_",j,"_",dataN[sz],".RData",sep=""))

		if (m==1) { bc<-bc1}
		if (m==2) { bc<-bc2}

		Xv<-x_valid[[j]][,-1]		
		
		bc_pred <- predict(bc, newdata=Xv, type="terms")
		tmp <- rbinom(bc_pred,1,bc_pred)
		bc_PAs <- array(tmp,dim=list(dim(bc_pred)[1],dim(bc_pred)[2],dim(bc_pred)[3]))

		bc_pred_w0sp<-array(NA,dim=list(nrow(y_valid[[j]]),ncol(y_valid[[j]]),dim(bc_pred)[3]),
							dimnames=list(1:nrow(y_valid[[j]]),colnames(y_valid[[j]]),1:dim(bc_pred)[3]))			

		for (i in 1:dim(bc_pred)[3]) {
			bc_pred_w0sp[,,i]<-rbinom(colMeans(y_train[[j]]),1,colMeans(y_train[[j]]))
			bc_pred_w0sp[,no0spNames,i]<-bc_PAs[,,i]
		}
		bc_PAs <- bc_pred_w0sp

		save(bc_PAs, file=paste(PD2,set_no,"/bc",m,"_PAs_",j,"_",dataN[sz],".RData",sep=""))

		rm(bc_pred)
		rm(bc_pred_w0sp)
		rm(bc_PAs)	
		gc()	
	}
	rm(no0spNames)
	gc()	
}	
##########################################################################################
	