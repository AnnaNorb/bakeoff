##########################################################################################
# BayesComm PREDICTIONS
##########################################################################################

require(BayesComm)

##########################################################################################

for (j in 1:3) {

	load(file=paste(FD,set_no,"/bc1_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/bc2_",j,"_",dataN[sz],".RData",sep=""))

	bc_pred <- predict(bc1, newdata=x_valid[[j]][,-1], type="response")
	bc2_pred <- predict(bc2, newdata=cbind(x_valid[[j]][,-1],s_valid[[j]]), type="response")

	Z1 <- array(NA,dim=list(dim(bc_pred)[1],dim(bc_pred)[2],REPs))
	Z1 <- array(NA,dim=list(dim(bc_pred)[1],dim(bc_pred)[2],REPs))
	Z2 <- Z1

	for (n in 1:REPs) {
		Z1[,,n] <- rnorm(bc_pred[,,n1],mean=bc_pred[,,n1],sd=1)
		Z2[,,n] <- rnorm(bc2_pred[,,n1],mean=bc2_pred[,,n1],sd=1)
		}		
	bc_PAs <- (Z1>0)*1
	bc2_PAs <- (Z2>0)*1
		
	save(bc_PAs, file=paste(PD2,set_no,"/bc1_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(bc2_PAs, file=paste(PD2,set_no,"/bc2_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(bc_pred)
	rm(bc2_pred)
	rm(Z1)
	rm(Z2)
	rm(bc_PAs)
	rm(bc2_PAs)
	
	gc()
		
	}
	
##########################################################################################
	