##########################################################################################
# BAYESIAN ORDINATIO AND REGRESSION ANALYSIS
##########################################################################################

require(boral)

##########################################################################################

mcmcControl<-list(n.burnin=5000,n.iteration=30000,n.thin=10,seed=10)

for (j in 1:3) {

	if (j==1) { sT<-Sys.time() }

	brl1	<-	boral(y=y_train[[j]], X=x_train[[j]][,-1], num.lv=0,
			  			family = "binomial", calc.ics = TRUE, 
			  			mcmc.control=mcmcControl, save.model = TRUE)

	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
	}	
	
	save(brl1, file=paste(FD,set_no,"/brl1_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_BORAL1_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }

	brl2	<-	boral(y=y_train[[j]], X=x_train[[j]][,-1], num.lv=2,
			  family = "binomial", calc.ics = TRUE, 
			  mcmc.control=mcmcControl, save.model = TRUE)

	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	
	save(brl2, file=paste(FD,set_no,"/brl2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_BORAL2_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }

	brl3	<-	boral(y=y_train[[j]], X=x_train[[j]][,-1], num.lv=2,
			  			family = "binomial", calc.ics = TRUE, row.eff = "random",
			  			mcmc.control=mcmcControl, save.model = TRUE)

	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	
	save(brl3, file=paste(FD,set_no,"/brl3_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_BORAL3_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }

	brl4	<-	boral(y=y_train[[j]], X=cbind(x_train[[j]][,-1],s_train[[j]]), num.lv=2,
			  			family = "binomial", calc.ics = TRUE,
			  			mcmc.control=mcmcControl, save.model = TRUE)

	if (j==1) { 
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	
	save(brl4, file=paste(FD,set_no,"/brl4_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_BORAL4_",dataN[sz],".RData",sep=""))
	}

}
