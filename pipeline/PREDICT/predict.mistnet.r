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

	mstnt1_probs <- predict(newnet,newdata=newnet$x,n_samples=REPs)

	mstnt1_PA <- rbinom(mstnt1_probs,1,mstnt1_probs)
	mstnt1_PAs <- array(mstnt1_PA, dim=list(dim(mstnt1_probs)[1],dim(mstnt1_probs)[2],dim(mstnt1_probs)[3]))

	save(mstnt1_PAs, file=paste(PD2,set_no,"/mstnt1_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(mstnt)
	rm(newnet)
	rm(mstnt1_probs)
	rm(mstnt1_PA)
	rm(mstnt1_PAs)
	gc()
	
	}
##########################################################################################
