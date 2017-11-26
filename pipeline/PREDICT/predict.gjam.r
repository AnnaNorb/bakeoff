##########################################################################################
# GENERALIZED JOINT ATTRIBUTE MODELS PREDICTIONS
##########################################################################################

require(gjam)

##########################################################################################

for (j in 1:3) {

	load(file=paste(FD,set_no,"/gjam_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/gjam2_",j,"_",dataN[sz],".RData",sep=""))

	Xv <- x_valid[[j]][,-1]
	colnames(Xv) <- letters[1:ncol(Xv)]
	newX <- list(xdata=as.data.frame(Xv))

	gjam_pred <- gjamPredict(gjam, newdata=newX)$prPresent
	gjam_pred2 <- gjamPredict(gjam2, newdata=newX)$prPresent

	gjam_PAs <- array(NA,dim=list(nrow(y_valid[[j]]),ncol(y_valid[[j]]),REPs))
	gjam2_PAs <- gjam_PAs
		
	for (n in 1:REPs) {
		gjam_PAs[,,n] <- rbinom(gjam_pred,1,gjam_pred)
		gjam2_PAs[,,n] <- rbinom(gjam_pred2,1,gjam_pred2)
	}

	save(gjam_PAs, file=paste(PD2,set_no,"/gjam_PAs_",j,"_",dataN[sz],".RData",sep=""))
	rm(gjam_PAs)
	
	save(gjam2_PAs, file=paste(PD2,set_no,"/gjam2_PAs_",j,"_",dataN[sz],".RData",sep=""))
	rm(gjam2_PAs)

	gc()

	}
	
##########################################################################################
