##########################################################################################
# STOCHASTIC FEED-FORWARD NEURAL NETWORK
##########################################################################################

require(mistnet2)

##########################################################################################

for (j in 1:3) {

	load(file=paste(FD,set_no,"/mstnt_",j,"_",dataN[sz],".RData",sep=""))

	Xv <- x_valid[[j]][,-1]

	newnet<-mstnt
	newnet$x<-Xv

	mstnt_probs <- predict(newnet,newdata=newnet$x,n_samples=REPs)

	mstnt_PA <- rbinom(mstnt_probs,1,mstnt_probs)
	mstnt_PAs <- array(mstnt_PA, dim=list(dim(mstnt_probs)[1],dim(mstnt_probs)[2],dim(mstnt_probs)[3]))

	save(mstnt_PAs, file=paste(PD2,set_no,"/mstnt_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(mstnt)
	rm(newnet)
	rm(mstnt_probs)
	rm(mstnt_PA)
	rm(mstnt_PAs)
	gc()
	
	}
##########################################################################################
