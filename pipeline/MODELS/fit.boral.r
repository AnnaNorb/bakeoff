##########################################################################################
# BAYESIAN ORDINATIO AND REGRESSION ANALYSIS
##########################################################################################

require(boral)

Nburn1<-30000
Niter1<-50000
Nthin1<-10
nSampls<-(Niter1-Nburn1)/Nthin1

mcmcControl1<-list(n.burnin=Nburn1,n.iteration=Niter1,n.thin=Nthin1,seed=7)

if (MCMC2) {
	mcmcControl1$seed <- 9
}
mcmcControl2<-mcmcControl1

if (sz > 1) {
	if (d==1) {
		Niter2<-20000
		Nthin2<-5
		Nburn2	<- Niter2-(nSampls*Nthin2)
		
	}
	if (d==2 | d==4) {
		Niter2<-50000
		Nthin2<-10
		Nburn2	<- Niter2-(nSampls*Nthin2)
	}
	if (d==3) {
		Niter2<-18000
		Nthin2<-5
		Nburn2	<- Niter2-(nSampls*Nthin2)
	}
	if (d==5) {
		Niter2<-11000
		Nthin2<-5
		Nburn2	<- Niter2-(nSampls*Nthin2)
	}
	mcmcControl2$n.burnin <- Nburn2
	mcmcControl2$n.iteration <- Niter2
	mcmcControl2$thin <- Nthin2
	
	if (MCMC2) {
		mcmcControl2$seed <- 9
	}
}


##########################################################################################

for (j in 1:3) {

	if (j==1) { sT<-Sys.time() }
	dir.create(file.path(FD,set_no,paste("boralModel1","_d",j,"_sz",sz,sep="")))
	setwd(paste(FD,set_no,paste("boralModel1","_d",j,"_sz",sz,sep="")))
	brl1	<-	boral(y=y_train[[j]], X=x_train[[j]][,-1], num.lv=0,
			  			family = "binomial", calc.ics = FALSE, 
			  			mcmc.control=mcmcControl1, save.model = TRUE)

	setwd(WD)
	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
	}	
	
	if (MCMC2) {
		save(brl1, file=file.path(FD,set_no,paste("brl1_",j,"_",dataN[sz],"_MCMC2.RData",sep="")))
		if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_BORAL1_",dataN[sz],"_MCMC2.RData",sep="")))
		}
	} else {
		save(brl1, file=file.path(FD,set_no,paste("brl1_",j,"_",dataN[sz],".RData",sep="")))
		if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_BORAL1_",dataN[sz],".RData",sep="")))
		}
	}

	if (j==1) { sT<-Sys.time() }
	dir.create(file.path(FD,set_no,paste("boralModel2","_d",j,"_sz",sz,sep="")))
	setwd(paste(FD,set_no,"/boralModel2","_d",j,"_sz",sz,sep=""))
	brl2	<-	boral(y=y_train[[j]], X=x_train[[j]][,-1], num.lv=2, 
			  		family = "binomial", calc.ics = FALSE,
			  		mcmc.control=mcmcControl2, save.model = TRUE)
	setwd(WD)

	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	if (MCMC2) {
		save(brl2, file=file.path(FD,set_no,paste("brl2_",j,"_",dataN[sz],"_MCMC2.RData",sep="")))
		if (j==1) {
			save(comTimes, file=file.path(FD,set_no,paste("comTimes_BORAL2_",dataN[sz],"_MCMC2.RData",sep="")))
		}
	} else {
		save(brl2, file=file.path(FD,set_no,paste("brl2_",j,"_",dataN[sz],".RData",sep="")))
		if (j==1) {
			save(comTimes, file=file.path(FD,set_no,paste("comTimes_BORAL2_",dataN[sz],".RData",sep="")))
		}
	}

}
