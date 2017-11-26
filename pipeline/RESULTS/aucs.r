##########################################################################################
# AREA UNDER CURVE
##########################################################################################

require(pROC)

load(file=paste(RD2,Sets[d],"/sp_occ_probs_",dataN[sz],".RData",sep=""))

AUCs <- matrix(NA,nrow=nmodels,ncol=3)

for (j in 1:3) {
	for (m in models) {

		AUCs[m,j] <- auc(predictor=as.vector(sp_occ_probs[[j]][,,m]),response=as.vector(y_valid[[j]]))

		}	
	}
	
save(AUCs, file=paste(RDfinal,dataN[sz],"/AUCs_",Sets[d],".RData",sep=""))


##########################################################################################


