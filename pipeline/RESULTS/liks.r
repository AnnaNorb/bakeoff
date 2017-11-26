##########################################################################################
# LIKELIHOOD
##########################################################################################

load(file=paste(RD2,Sets[d],"/sp_occ_probs_",dataN[sz],".RData",sep=""))

LIKs <- matrix(NA,nrow=nmodels,ncol=3)

for (j in 1:3) {

	PROBs<-(sp_occ_probs[[j]]*0.99)+0.005

	for (m in models) {

		LIKs[m,j] <- sum((y_valid[[j]]*log(PROBs[,,m])+(1-y_valid[[j]])*log(1-PROBs[,,m])),na.rm=T)/(dim(y_valid[[j]])[1]*dim(y_valid[[j]])[2])
	
		}	
		rm(PROBs)   
		gc()   
	}
	
save(LIKs, file=paste(RDfinal,dataN[sz],"/LIKs_",Sets[d],".RData",sep=""))


##########################################################################################


