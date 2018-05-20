##########################################################################################
# MULTIVARIATE ADAPTIVE REGRESSION SPLINES PREDICTIONS
##########################################################################################

require(earth)

##########################################################################################

for (j in 1:3) {

	Xv <- x_valid[[j]][,-1]
	Xvs <- cbind(x_valid[[j]][,-1],s_valid[[j]])
	nsites <- nrow(y_valid[[j]])
	nsp <- ncol(y_valid[[j]])
	
	load(file=paste(FD,set_no,"/mars1_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/mars2_",j,"_",dataN[sz],".RData",sep=""))

	mars1_preds <- predict(mars1, newdata=Xv)
	mars2_preds <- predict(mars2, newdata=Xv)

	Z1 <- array(NA, dim=list(nsites,nsp,REPs), dimnames=list(1:nsites,dimnames(mars1_preds)[[2]],1:REPs))
	Z2 <- array(NA, dim=list(nsites,nsp,REPs), dimnames=list(1:nsites,dimnames(mars2_preds)[[2]],1:REPs))

	for (n in 1:REPs) {
		Z1[,,n] <- rnorm(mars1_preds,mean=mars1_preds,sd=1)
		Z2[,,n] <- rnorm(mars2_preds,mean=mars2_preds,sd=1)
	}
	mars1_PAs <- (Z1>0)*1
	mars2_PAs <- (Z2>0)*1

	rm(mars1_preds)
	rm(mars2_preds)
	rm(Z1)
	rm(Z2)

	save(mars1_PAs, file=paste(PD2,set_no,"/mars1_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(mars2_PAs, file=paste(PD2,set_no,"/mars2_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(mars1_PAs)
	rm(mars2_PAs)
	gc()
	
	}
##########################################################################################

