##########################################################################################
# STOCHASTIC FEED-FORWARD NEURAL NETWORK
##########################################################################################

require(mistnet2)

##########################################################################################

for (j in 1:3) {

	for (m in 1:2) {

		load(file=file.path(FD,set_no,paste("mstnt",m,"_",j,"_",dataN[sz],".RData",sep="")))

		Xv <- x_valid[[j]][,-1]
	
		if (m==1) { newnet<-mstnt1 }
		if (m==2) { newnet<-mstnt2 }

		newnet$x<-Xv

		mstnt_probs <- predict(newnet,newdata=newnet$x,n_samples=REPs)

		mstnt_PA <- rbinom(mstnt_probs,1,mstnt_probs)
		mstnt_PAs <- array(mstnt_PA, dim=list(dim(mstnt_probs)[1],dim(mstnt_probs)[2],dim(mstnt_probs)[3]))

		save(mstnt_PAs, file=file.path(PD2,set_no,paste("mstnt",m,"_PAs_",j,"_",dataN[sz],".RData",sep="")))

		if (m==1) { rm(mstnt1) }
		if (m==2) { rm(mstnt2) }
		rm(newnet)
		rm(mstnt_probs)
		rm(mstnt_PA)
		rm(mstnt_PAs)
		gc()
	
	}
}	
##########################################################################################
