##########################################################################################
# BOOSTED REGRESSION TREES prediction
##########################################################################################

require("gbm")
require("dismo")

##########################################################################################

for (j in 1:3) {

	load(file=file.path(FD,set_no,paste("brt1_",j,"_",dataN[sz],".RData",sep="")))
	newX<-as.data.frame(x_valid[[j]])

	probmat <- rep(colMeans(y_train[[j]]),times=rep(nrow(y_valid[[j]]),times=ncol(y_valid[[j]])))
	brt1_PAs <- array(NA, dim=list(nrow(y_valid[[j]]),ncol(y_valid[[j]]),REPs))

	for (n in 1:REPs) {
		Probs <- matrix(probmat, ncol=ncol(y_valid[[j]]), nrow=nrow(y_valid[[j]]))
		
		for (i in 1:length(brt1)) {
			brt <- brt1[[i]]
			if (is.null(brt)==FALSE & is.na(brt[1])==FALSE) {
				Probs[,i] <- predict(brt, n.trees=brt$gbm.call$best.trees, type="response", 
									newdata=newX)
			}
		rm(brt)
		}
		brt1_PAs[,,n] <- matrix(rbinom(length(Probs),1,Probs),ncol=ncol(Probs))
	}
	save(brt1_PAs, file=file.path(PD2,set_no,paste("brt1_PAs_",j,"_",dataN[sz],".RData",sep="")))

	rm(probmat)
	rm(Probs)
	rm(brt1_PAs)
	rm(brt1)
	gc()
		
}	
##########################################################################################
