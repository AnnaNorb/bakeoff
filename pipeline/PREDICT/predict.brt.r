##########################################################################################
# BOOSTED REGRESSION TREES prediction
##########################################################################################

require("gbm")
require("dismo")

##########################################################################################

for (j in 1:3) {

	load(file=paste(FD,set_no,"/brt_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/brt2_",j,"_",dataN[sz],".RData",sep=""))

	probmat <- rep(colMeans(y_train[[j]]),times=rep(nrow(y_valid[[j]]),times=ncol(y_valid[[j]])))

	brt_PAs <- array(NA, dim=list(nrow(y_valid[[j]]),ncol(y_valid[[j]]),REPs))
	brt2_PAs <- array(NA, dim=list(nrow(y_valid[[j]]),ncol(y_valid[[j]]),REPs))

	for (n in 1:REPs) {
		Probs <- matrix(probmat, ncol=ncol(y_valid[[j]]), nrow=nrow(y_valid[[j]]))
		Probs2 <- matrix(probmat, ncol=ncol(y_valid[[j]]), nrow=nrow(y_valid[[j]]))
			
			for (i in 1:length(brt)) {
				gbmModel <- brt[[i]]
				if (is.null(gbmModel)==FALSE & is.na(gbmModel[1])==FALSE) {
  					Probs[,i] <- predict(gbmModel, n.trees=gbmModel$gbm.call$best.trees, type="response", 
  										newdata=as.data.frame(x_valid[[j]]))
					}

				gbmModel2 <- brt2[[i]]
				if (is.null(gbmModel2)==FALSE & is.na(gbmModel2[1])==FALSE) {
  					Probs2[,i] <- predict(gbmModel2, n.trees=gbmModel2$gbm.call$best.trees, type="response", 
  										newdata=as.data.frame(cbind(x_valid[[j]],s_valid[[j]])))
					}
				}
		brt_PAs[,,n] <- matrix(rbinom(length(Probs),1,Probs),ncol=ncol(Probs))
		brt2_PAs[,,n] <- matrix(rbinom(length(Probs2),1,Probs2),ncol=ncol(Probs2))
		}
		
	save(brt_PAs, file=paste(PD2,set_no,"/brt_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(brt2_PAs, file=paste(PD2,set_no,"/brt2_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(probmat)
	rm(Probs)
	rm(Probs2)
	rm(gbmModel)
	rm(gbmModel2)
	rm(brt_PAs)
	rm(brt2_PAs)
	rm(brt)
	rm(brt2)
	gc()
	
	}	
	
##########################################################################################
