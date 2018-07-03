##########################################################################################
# RANDOM FORESTS
##########################################################################################

require(randomForest)
require(e1071)

##########################################################################################

ntrees<-c(500,1000,1500,2000)
ncovar<-(ncol(x_train[[1]])-1)/2
mtrys=2:ceiling(ncovar/2)
nodszs<-c(1,2,5,10)

for (j in 1:3) {

	nsp <- length(DD_t[[j]])

	DD_t_rf <- DD_t[[j]]
	for (k in 1:nsp) {
		DD_t_rf[[k]]<-DD_t_rf[[k]][,c(1,3:(ncovar+2))]
		#DD_t_rf[[k]][,1]<-as.factor(DD_t[[j]][[k]][,1])
	}

# 	if (j==1) { sT<-Sys.time() }
# 	for (k in 1:nsp) {
# 		tuneResult <- NULL
# 		tuneResult <- try(tune(randomForest, sp ~ ., data=DD_t_rf[[k]],
# 										importance=TRUE,
# 										keep.forest=TRUE,
# 										proximity=TRUE,
# 										ranges=list(ntree=ntrees,mtry=mtrys)))

	if (j==1) { sT<-Sys.time() }
	for (k in 1:nsp) {
		tuneResult <- NULL
		tuneResult <- try(tune(randomForest, sp ~ ., data=DD_t_rf[[k]],
										importance=TRUE,
										keep.forest=TRUE,
										proximity=TRUE,
										ranges=list(ntree=ntrees,mtry=mtrys, nodesize=nodszs)))

# 		tuneResult <- try(best.randomForest(sp ~ ., data=DD_t_rf[[k]],
# 										importance=TRUE,
# 										keep.forest=TRUE,
# 										proximity=TRUE,ntree=ntrees,mtry=mtrys))
# 										#ranges=list(ntree=ntrees,mtry=mtrys)))


# 		if (is(tuneResult)=="randomForest.formula") {	
		if (inherits(tuneResult,"tune")) {	
			rff1<-tuneResult$best.model
		} else {
			rff1<-NULL	
		}
		save(rff1, file=file.path(FD,set_no,"rfs",paste("rf1_",j,"_sp",k,"_",dataN[sz],".RData",sep="")))
		rm(rff1)
		gc()
	}
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	if (j==1) {
	save(comTimes, file=file.path(FD,set_no,paste("comTimes_RF1_",dataN[sz],".RData",sep="")))
	rm(comTimes)
	}
}

##########################################################################################