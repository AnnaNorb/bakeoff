##########################################################################################
# SUPPORT VECTOR MACHINES
##########################################################################################

require(e1071)

##########################################################################################

costs<-c(1,5,10,50,100)
gammas<-c(0.001,0.01,0.1,1,10,100)

##########################################################################################

for (j in 1:3) {

	nsp <- length(DD_t[[j]])
	ncovar<-(ncol(x_train[[j]])-1)/2

	DD_t_svm <- DD_t[[j]]
	for (i in 1:length(DD_t_svm)) {
		DD_t_svm[[i]]<-DD_t_svm[[i]][,c(1,3:(ncovar+2))]
		#DD_t_svm[[i]][,1]<-as.factor(DD_t[[j]][[i]][,1])
	}

# 	svmf <- list()
# 	if (j==1) { sT<-Sys.time() }
# 	for (k in 1:nsp) {
# 		tuneResult <- NULL
# 		tuneResult <- try(tune(svm, sp ~ ., data=DD_t_svm[[k]],
# 						scale=FALSE,probability=TRUE,
# 						type='C-classification',
# 						tunecontrol=tune.control(sampling="cross",cross=10),
# 						ranges=list(gamma=gammas, cost=costs)))					

	svmf1 <- list()
	if (j==1) { sT<-Sys.time() }
	for (k in 1:nsp) {
		tuneResult <- NULL
		tuneResult <- try(tune(svm, sp ~ ., data=DD_t_svm[[k]],
						scale=FALSE,probability=TRUE,
						type='eps-regression',
						tunecontrol=tune.control(sampling="cross",cross=10),
						ranges=list(gamma=gammas, cost=costs)))		

		if (inherits(tuneResult,"tune")) {	
			svmf1[[k]]<-tuneResult$best.model
		} else {
			svmf1[[k]]<-NULL	
		}
	}

	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
	}
	save(svmf1, file=file.path(FD,set_no,paste("svm1_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_SVM1_",dataN[sz],".RData",sep="")))
		rm(comTimes)
	}

	rm(svmf1)		
	rm(DD_t_svm)		
	gc()
}
	
##########################################################################################
