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
	
	load(file=paste(FD,set_no,"/mars_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/mars_int_",j,"_",dataN[sz],".RData",sep=""))

	mars_preds <- predict(mars, newdata=Xv)
	mars_int_preds <- predict(mars_int, newdata=Xv)

	Z <- array(NA, dim=list(nsites,nsp,REPs), dimnames=list(1:nsites,dimnames(mars_preds)[[2]],1:REPs))
	Z_int <- array(NA, dim=list(nsites,nsp,REPs), dimnames=list(1:nsites,dimnames(mars_int_preds)[[2]],1:REPs))

	for (n in 1:REPs) {
		Z[,,n] <- rnorm(mars_preds,mean=mars_preds,sd=1)
		Z_int[,,n] <- rnorm(mars_int_preds,mean=mars_int_preds,sd=1)
		}
	mars_PAs <- (Z>0)*1
	mars_int_PAs <- (Z_int>0)*1

	rm(mars_preds)
	rm(mars_int_preds)
	rm(Z)
	rm(Z_int)

	save(mars_PAs, file=paste(PD2,set_no,"/mars_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(mars_int_PAs, file=paste(PD2,set_no,"/mars_int_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(mars_PAs)
	rm(mars_int_PAs)
	gc()
	
	}
##########################################################################################

