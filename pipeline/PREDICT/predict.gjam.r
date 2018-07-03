##########################################################################################
# GENERALIZED JOINT ATTRIBUTE MODELS PREDICTIONS
##########################################################################################

require(gjam)

##########################################################################################

for (j in 1:3) {	
	load(file=file.path(FD,set_no,paste("no0sp_GJAM_",j,"_",dataN[sz],".RData",sep="")))
	load(file=file.path(FD,set_no,paste("gjam1_",j,"_",dataN[sz],".RData",sep="")))

	gjamMod<-gjam1
	Xv <- x_valid[[j]][,-1]
	colnames(Xv) <- letters[1:ncol(Xv)]
	newX <- list(xdata=as.data.frame(Xv), nsim=REPs)

	gjam_pred <- gjamPredict(gjamMod, newdata=newX, FULL=TRUE)	
		
	gjam_pred_w0sp<-array(NA,dim=list(nrow(y_valid[[j]]),ncol(y_valid[[j]]),REPs),
								dimnames=list(1:nrow(y_valid[[j]]),colnames(y_valid[[j]]),1:REPs))			
	for (i in 1:REPs) {
		gjam_pred_w0sp[,,i]<-rbinom(rep(colMeans(y_train[[j]]),each=nrow(y_train[[j]])),1,rep(colMeans(y_train[[j]]),each=nrow(y_train[[j]])))
		tmp<-matrix(gjam_pred$ychains[i,],nrow(Xv),length(no0spNames))
		gjam_pred_w0sp[,no0spNames,i]<-rbinom(tmp,1,tmp)
		#gjam_pred_w0sp[,no0spNames,i]<-gjam_pred[,((i-1)*length(no0spNames)+1):(i*length(no0spNames))]
	}
	gjam1_PAs<-gjam_pred_w0sp
	
	save(gjam1_PAs, file=file.path(PD2,set_no,paste("gjam1_PAs_",j,"_",dataN[sz],".RData",sep="")))

	rm(gjam_pred)
	rm(gjam_pred_w0sp)
	rm(gjam1_PAs)
	rm(no0spNames)
	gc()
}	

##########################################################################################
