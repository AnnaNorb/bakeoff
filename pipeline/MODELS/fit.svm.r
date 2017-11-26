##########################################################################################
# SUPPORT VECTOR MACHINES
##########################################################################################

require(e1071)

##########################################################################################

costs<-c(1,5,10,50,100)
gammas<-c(100,10,1,0.1,0.01)

for (j in 1:3) {

	nsp <- length(DD_t[[j]])
	ncovar <- ncol(DD_t[[j]][[1]])-2

	DD_t_svm <- DD_t[[j]]
	for (i in 1:length(DD_t_svm)) {
		DD_t_svm[[i]][,1]<-as.factor(DD_t[[j]][[i]][,1])
	}


	svmf <- list()
		if (j==1) { sT<-Sys.time() }
		for (i in 1:nsp) {

			tuneResult <- try(tune(svm, sp ~ ., data=DD_t_svm[[i]][,c(1,3:ncovar)],
							scale=FALSE,probability=TRUE,
							cross=10, type='C-classification',
							tunecontrol=tune.control(sampling="cross"),
							ranges=list(gamma=gammas, cost=costs)))
			
			if (is(tuneResult)=="tune") {	
				svmf[[i]]<-tuneResult$best.model
			} else {
				svmf[[i]]<-NULL	
			}
		}
		if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
		save(svmf, file=paste(FD,set_no,"/svm_",j,"_",dataN[sz],".RData",sep=""))
		if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_SVM_",dataN[sz],".RData",sep=""))
		}


	svmf2 <- list()
		if (j==1) { sT<-Sys.time() }
		for (i in 1:nsp) {

			tuneResult <- try(tune(svm, sp ~ ., data=DD_t_svm[[i]][,c(1,3:(ncovar+2))],
							scale=FALSE,probability=TRUE,
							cross=10, type='C-classification',
							tunecontrol=tune.control(sampling="cross"),
							ranges=list(gamma=gammas, cost=costs)))
			
			if (is(tuneResult)=="tune") {	
				svmf2[[i]]<-tuneResult$best.model
			} else {
				svmf2[[i]]<-NULL	
			}
		}
		if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
		save(svmf2, file=paste(FD,set_no,"/svm2_",j,"_",dataN[sz],".RData",sep=""))
		if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_SVM2_",dataN[sz],".RData",sep=""))
		}
	}
	
##########################################################################################
