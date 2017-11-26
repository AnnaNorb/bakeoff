##########################################################################################
# STOCHASTIC FEED-FORWARD NEURAL NETWORK
##########################################################################################

require(mistnet2)

##########################################################################################

for (j in 1:3) {

	load(file=paste(FD,set_no,"/mstnt_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/mstnt2_",j,"_",dataN[sz],".RData",sep=""))

	Xv <- x_valid[[j]][,-1]
	Xvs <- cbind(Xv,s_valid[[j]])
	Yv <- y_valid[[j]]

	newnet<-mstnt
	newnet$x<-Xv
	newnet$y<-Yv
	newnet2<-mstnt2
	newnet2$x<-Xvs
	newnet2$y<-Yv

	mstnt_probs <- predict(newnet,newdata=newnet$x,n_samples=REPs)
	mstnt2_probs <- predict(newnet2,newdata=newnet2$x,n_samples=REPs)

	mstnt_PA <- rbinom(mstnt_probs,1,mstnt_probs)
	mstnt_PAs <- array(mstnt_PA, dim=list(dim(mstnt_probs)[1],dim(mstnt_probs)[2],dim(mstnt_probs)[3]))
	mstnt2_PA <- rbinom(mstnt2_probs,1,mstnt2_probs)
	mstnt2_PAs <- array(mstnt2_PA, dim=list(dim(mstnt2_probs)[1],dim(mstnt2_probs)[2],dim(mstnt2_probs)[3]))

	save(mstnt_PAs, file=paste(PD2,set_no,"/mstnt_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(mstnt2_PAs, file=paste(PD2,set_no,"/mstnt2_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(mstnt)
	rm(mstnt2)
	rm(newnet)
	rm(newnet2)
	rm(mstnt_probs)
	rm(mstnt2_probs)
	rm(mstnt_PA)
	rm(mstnt2_PA)
	rm(mstnt_PAs)
	rm(mstnt2_PAs)
	gc()
	
	}
##########################################################################################
