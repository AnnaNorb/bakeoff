##########################################################################################
# RANDOM FORESTS
##########################################################################################

require(randomForest)
require(e1071)

##########################################################################################

ntrees<-c(500,1000,1500,2000)
ncovar<-ncol(x_train[[1]])-1
mtrys=2:(ncovar/2)

for (j in 1:3) {

	nsp <- length(DD_t[[j]])

	DD_t_rf <- DD_t[[j]]
	for (i in 1:length(DD_t_rf)) {
		DD_t_rf[[i]][,1]<-as.factor(DD_t[[j]][[i]][,1])
	}

	if (j==1) { sT<-Sys.time() }
	for (k in 1:nsp) {
		tuneResult <- try(tune(randomForest, sp ~ ., data=DD_t_rf[[k]][,c(1,3:(2+ncovar))],
										importance=TRUE,
										keep.forest=TRUE,
										proximity=TRUE,
										ranges=list(ntree=ntrees,mtry=mtrys)))

		if (is(tuneResult)=="tune") {	
			rff<-tuneResult$best.model
			} else {
			rff<-NULL	
			}
		save(rff, file=paste(FD,set_no,"/rfs/rf_",j,"_sp",k,"_",dataN[sz],".RData",sep=""))
		rm(rff)
		gc()
	}
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_RF_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
	for (k in 1:nsp) {
		tuneResult <- try(tune(randomForest, sp ~ ., data=DD_t_rf[[k]][,c(1,3:ncol(DD_t_rf[[k]]))],
										importance=TRUE,
										keep.forest=TRUE,
										proximity=TRUE,
										ranges=list(ntree=ntrees,mtry=mtrys)))
		if (is(tuneResult)=="tune") {	
			rff2<-tuneResult$best.model
			} else {
			rff2<-NULL	
			}
		save(rff2, file=paste(FD,set_no,"/rfs2/rf2_",j,"_sp",k,"_",dataN[sz],".RData",sep=""))
		rm(rff2)
		gc()
	}
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}	
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_RF2_",dataN[sz],".RData",sep=""))
	}

}

##########################################################################################