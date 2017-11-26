# SUPPORT VECTOR MACHINES
##########################################################################################

require("e1071")

costs<-c(1,2,2^(2:9))

##########################################################################################

for (j in 1:3) {

	gammas<-c(10/nrow(DD_t[[j]][[1]]),5/nrow(DD_t[[j]][[1]]),1/nrow(DD_t[[j]][[1]]),0.1/nrow(DD_t[[j]][[1]]),0.005/nrow(DD_t[[j]][[1]]))
	nsp <- length(DD_t[[j]])
	ncovar <- ncol(DD_t[[j]][[1]])-2

	DD_t_svm <- DD_t[[j]]
	for (i in 1:length(DD_t_svm)) {
		DD_t_svm[[i]][,1]<-as.factor(DD_t[[j]][[i]][,1])
	}

	svmf <- list()
	sT<-Sys.time()

		for (i in 1:nsp) {
			
			tuneResult <- try(tune(svm, sp ~ ., data=DD_t_svm[[i]][,c(1,3:(2+ncovar))],
							scale=FALSE,probability=TRUE,
							cross=10, type='C-classification', kernel="polynomial",
							ranges=list(gamma=gammas, cost=costs)))
			
			if (is(tuneResult)=="tune") {
			svmf[[i]]<-tuneResult$best.model
			} else {
			svmf[[i]]<-NULL	
			}
		}
		eT<-Sys.time()
		comTimes$SVM<-eT-sT

save(svmf, file=paste(FD,set_no,"/svm_",j,".RData",sep=""))
}
##########################################################################################
