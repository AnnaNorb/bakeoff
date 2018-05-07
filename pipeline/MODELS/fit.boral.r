##########################################################################################
# BAYESIAN ORDINATIO AND REGRESSION ANALYSIS
##########################################################################################

require(boral)

##########################################################################################

#if (MCMC2) {
#	mcmcControl<-list(n.burnin=90000,n.iteration=100000,n.thin=100,seed=7)
#} else {
#	mcmcControl<-list(n.burnin=40000,n.iteration=50000,n.thin=100,seed=7)
#}
if (MCMC2) {
	mcmcControl<-list(n.burnin=20000,n.iteration=40000,n.thin=200,seed=7)
} else {
	mcmcControl<-list(n.burnin=10000,n.iteration=20000,n.thin=100,seed=7)
}
	
##########################################################################################

for (j in 1:3) {

	if (j==1) { sT<-Sys.time() }
	dir.create(paste(FD,set_no,"/boralModel1","_d",j,"_sz",sz,sep="")); setwd(paste(FD,set_no,"/boralModel1","_d",j,"_sz",sz,sep=""))
	brl1	<-	boral(y=y_train[[j]], X=x_train[[j]][,-1], num.lv=0,
			  			family = "binomial", calc.ics = FALSE, 
			  			mcmc.control=mcmcControl, save.model = TRUE)
	setwd(WD)
	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
	}	
	
	if (MCMC2) {
		save(brl1, file=paste(FD,set_no,"/brl1_",j,"_",dataN[sz],"_MCMC2.RData",sep=""))
		if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_BORAL1_",dataN[sz],"_MCMC2.RData",sep=""))
		}
	} else {
		save(brl1, file=paste(FD,set_no,"/brl1_",j,"_",dataN[sz],".RData",sep=""))
		if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_BORAL1_",dataN[sz],".RData",sep=""))
		}
	}

	if (j==1) { sT<-Sys.time() }
	dir.create(paste(FD,set_no,"/boralModel2","_d",j,"_sz",sz,sep="")); setwd(paste(FD,set_no,"/boralModel2","_d",j,"_sz",sz,sep=""))
	brl2	<-	boral(y=y_train[[j]], X=x_train[[j]][,-1], num.lv=2, 
			  		family = "binomial", calc.ics = FALSE,
			  		mcmc.control=mcmcControl, save.model = TRUE)
	setwd(WD)

	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	if (MCMC2) {
		save(brl2, file=paste(FD,set_no,"/brl2_",j,"_",dataN[sz],"_MCMC2.RData",sep=""))
		if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_BORAL2_",dataN[sz],"_MCMC2.RData",sep=""))
		}
	} else {
		save(brl2, file=paste(FD,set_no,"/brl2_",j,"_",dataN[sz],".RData",sep=""))
		if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_BORAL2_",dataN[sz],".RData",sep=""))
		}
	}

}
