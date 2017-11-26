##########################################################################################
# BayesComm ANALYSIS
##########################################################################################

require(BayesComm)

##########################################################################################

mcmcControl<-list(nburn=20000,niter=40000,nthin=400)

for (j in 1:3) {

	if (j==1) { sT<-Sys.time() }

	bc1	<-	BC(Y=y_train[[j]], X=x_train[[j]][,-1], model="full",
   					its=mcmcControl$niter, thin=mcmcControl$nthin, burn=mcmcControl$nburn)

	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
	}	
	
	save(bc1, file=paste(FD,set_no,"/bc1_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_BC1_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }

	bc2	<-	BC(Y=y_train[[j]], X=cbind(x_train[[j]][,-1],s_train[[j]]), model="full",
   					its=mcmcControl$niter, thin=mcmcControl$nthin, burn=mcmcControl$nburn)

	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
	}	
	
	save(bc2, file=paste(FD,set_no,"/bc2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_BC2_",dataN[sz],".RData",sep=""))
	}

}
