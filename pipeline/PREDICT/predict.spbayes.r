##########################################################################################
# BAYESIAN ORDINATIO AND REGRESSION ANALYSIS PREDICTIONS
##########################################################################################

require(spBayes)

##########################################################################################

for (j in 1:3) {

	load(file=paste(FD,set_no,"/spBspat_",j,"_",dataN[sz],".RData",sep=""))

	Xmv <- mkMvX(lapply(seq_len(ncol(y_valid[[j]])), function(X) x_valid[[j]]))
	Xmv2 <- mkMvX(lapply(seq_len(ncol(y_valid[[j]])), function(X) x_valid[[j]][,-1]))

	nmcmc <- nrow(spBspat$p.beta.theta.samples)

	spBspat_preds <- mySpPredict(sp.obj=spBspat,pred.coords=s_valid[[j]],pred.covars=Xmv2, 
							   start=(nmcmc/2)+1, end=nmcmc, thin=10)

	Z1 <- array(NA,dim=list(nsites,nsp,REPs))
	for (n in 1:REPs) {
		Z1[,,n] <- rnorm(brl1_preds,mean=brl1_preds,sd=1)
		}		
	boral1_PAs <- (Z1>0)*1
		
	save(boral1_PAs, file=paste(PD2,set_no,"/boral1_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(brl1)
	rm(brl1_preds)
	rm(Z1)
	rm(boral1_PAs)
	
	gc()
		
	}
	
##########################################################################################
	