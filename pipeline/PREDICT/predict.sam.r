##########################################################################################
# SPECIES ARCHETYPE MODELS PREDICTIONS
##########################################################################################

require(mvabund)
source(paste(MD,"coordinSAMsv2.R",sep=""))

##########################################################################################

for (j in 1:3) {

	nsp <- ncol(y_valid[[j]])
	nsites <- nrow(y_valid[[j]])

	Xv <- x_valid[[j]][,-1]
	Xvs <- cbind(Xv,s_valid[[j]])

	load(file=paste(FD,set_no,"/sams_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/sams2_",j,"_",dataN[sz],".RData",sep=""))

	sam_PAs <- array(NA, dim=list(nsites,nsp,REPs))
	sam2_PAs <- sam_PAs

	for (n in 1:REPs) {
		sam_preds <- predict.sams(psams=sams, newX=Xv, family="binomial")
		sam2_preds <- predict.sams(psams=sams2, newX=Xv, family="binomial")
		sam_PAs[,,n] <- sam_preds$predict.y
		sam2_PAs[,,n] <- sam2_preds$predict.y
		}

	rm(sams)
	rm(sams2)
	rm(sam_preds)
	rm(sam2_preds)

	save(sam_PAs, file=paste(PD2,set_no,"/sam_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(sam2_PAs, file=paste(PD2,set_no,"/sam2_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(sam_PAs)
	rm(sam2_PAs)
	
	gc()
	
	}
##########################################################################################
