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

	load(file=paste(FD,set_no,"/sams1_",j,"_",dataN[sz],".RData",sep=""))

	sam1_PAs <- array(NA, dim=list(nsites,nsp,REPs))

	for (n in 1:REPs) {
		sam1_preds <- predict.sams(psams=sams1, newX=Xv, family="binomial")
		sam1_PAs[,,n] <- sam1_preds$predict.y
		}

	save(sam1_PAs, file=paste(PD2,set_no,"/sam1_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(sams1)
	rm(sam1_preds)
	rm(sam_PAs)	
	gc()
	
	}
##########################################################################################
