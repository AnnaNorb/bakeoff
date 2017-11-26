##########################################################################################
# BAYESIAN ORDINATIO AND REGRESSION ANALYSIS PREDICTIONS
##########################################################################################

require(boral)

##########################################################################################

for (j in 1:3) {

	Xv <- x_valid[[j]][,-1]
	nsites <- nrow(y_train[[j]])
	nsp <- ncol(y_valid[[j]])
	#newIDs <- (nsites+1):(nsites+nrow(y_valid[[j]]))

	load(file=paste(FD,set_no,"/brl1_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/brl2_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/brl3_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/brl4_",j,"_",dataN[sz],".RData",sep=""))

	brl1_preds <- predict(brl1, newX=Xv)
	brl2_preds <- predict(brl2, newX=Xv, predict.type="marginal")
	brl3_preds <- predict(brl3, newX=Xv, predict.type="marginal")	
	brl4_preds <- predict(brl4, newX=Xv, predict.type="marginal")

	Z1 <- array(NA,dim=list(nsites,nsp,REPs))
	Z2 <- Z1
	Z3 <- Z1
	Z4 <- Z1
	for (n in 1:REPs) {
		Z1[,,n] <- rnorm(brl1_preds$linpred,mean=brl1_preds$linpred,sd=1)
		Z2[,,n] <- rnorm(brl2_preds$linpred,mean=brl2_preds$linpred,sd=1)
		Z3[,,n] <- rnorm(brl3_preds$linpred,mean=brl3_preds$linpred,sd=1)
		Z4[,,n] <- rnorm(brl4_preds$linpred,mean=brl4_preds$linpred,sd=1)
		}		
	boral1_PAs <- (Z1>0)*1
	boral2_PAs <- (Z2>0)*1
	boral3_PAs <- (Z3>0)*1
	boral4_PAs <- (Z4>0)*1

	save(boral1_PAs, file=paste(PD2,set_no,"/boral1_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(boral2_PAs, file=paste(PD2,set_no,"/boral2_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(boral3_PAs, file=paste(PD2,set_no,"/boral3_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(boral4_PAs, file=paste(PD2,set_no,"/boral4_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(brl1)
	rm(brl2)
	rm(brl3)
	rm(brl4)
	rm(brl1_preds)
	rm(brl2_preds)
	rm(brl3_preds)
	rm(brl4_preds)
	rm(Z1)
	rm(Z2)
	rm(Z3)
	rm(Z4)
	rm(boral1_PAs)
	rm(boral2_PAs)
	rm(boral3_PAs)
	rm(boral4_PAs)
	
	gc()
		
	}
	
##########################################################################################
	