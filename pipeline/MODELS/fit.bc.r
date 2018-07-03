##########################################################################################
# BayesComm ANALYSIS
##########################################################################################

require(BayesComm)

##########################################################################################

if (MCMC2) {
	mcmcControl<-list(nburn=80000,niter=100000,nthin=200)
} else {
	mcmcControl<-list(nburn=40000,niter=50000,nthin=100)
}

##########################################################################################

for (j in 1:3) {

	no0sp<-(colSums(y_train[[j]])!=0 & colSums(y_train[[j]])!=dataN[sz])
	y_train_no0sp<-y_train[[j]][,no0sp]
	if ( sum(no0sp)>=dataN[sz] ) {
		dif <- -(dataN[sz]-sum(no0sp)-1)
		drp<-order(colSums(y_train_no0sp))[1:dif]
		y_train_no0sp<-y_train_no0sp[,-drp]
	}
	no0spNames<-colnames(y_train_no0sp)
	save(no0spNames, file=file.path(FD,set_no,paste("no0sp_BC_",j,"_",dataN[sz],".RData",sep="")))

	if (j==1) { sT<-Sys.time() }
		#bc1	<-	try(BC(Y=y_train[[j]], X=x_train[[j]][,-1], model="environment",
	   	#				its=mcmcControl$niter, thin=mcmcControl$nthin, burn=mcmcControl$nburn))
	  	#	if (is(bc1)!="bayescomm") {
		bc1	<-	BC(Y=y_train_no0sp, X=x_train[[j]][,-1], model="environment",
				its=mcmcControl$niter, thin=mcmcControl$nthin, burn=mcmcControl$nburn)
   					
	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
	}	
	if (MCMC2) {
		save(bc1, file=file.path(FD,set_no,paste("bc1_",j,"_",dataN[sz],"_MCMC2.RData",sep="")))
		if (j==1) {
			save(comTimes, file=file.path(FD,set_no,paste("comTimes_BC1_",dataN[sz],"_MCMC2.RData",sep="")))
		}
	} else {
		save(bc1, file=file.path(FD,set_no,paste("bc1_",j,"_",dataN[sz],".RData",sep="")))
		if (j==1) {
			save(comTimes, file=file.path(FD,set_no,paste("comTimes_BC1_",dataN[sz],".RData",sep="")))
		}
	}

	if (j==1) { sT<-Sys.time() }
		bc2	<-	BC(Y=y_train_no0sp, X=x_train[[j]][,-1], model="full",
				its=mcmcControl$niter, thin=mcmcControl$nthin, burn=mcmcControl$nburn)

	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
	}	
	if (MCMC2) {
		save(bc2, file=file.path(FD,set_no,paste("bc2_",j,"_",dataN[sz],"_MCMC2.RData",sep="")))
		if (j==1) {
			save(comTimes, file=file.path(FD,set_no,paste("comTimes_BC2_",dataN[sz],"_MCMC2.RData",sep="")))
		}
	} else {
		save(bc2, file=file.path(FD,set_no,paste("bc2_",j,"_",dataN[sz],".RData",sep="")))
		if (j==1) {
			save(comTimes, file=file.path(FD,set_no,paste("comTimes_BC2_",dataN[sz],".RData",sep="")))
		}
	}

rm(bc1)
rm(bc2)
gc()
}
