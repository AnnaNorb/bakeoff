##########################################################################################
# MULTIVARIATE REGRESSION TREE
##########################################################################################

require(mvpart)
require(caret)

##########################################################################################

for (j in 1:3) {

	nsp <- ncol(y_train[[j]])
	nsites <- nrow(y_train[[j]])
	ncovar<-(ncol(x_train[[1]])-1)/2

	dataAll<-as.data.frame(cbind(y_train[[j]],x_train[[j]][,c(2:(ncovar+1))]))
	colnames(dataAll)<-paste("V",1:ncol(dataAll),sep="")

	if (j==1) { sT<-Sys.time() }
	mrts <- mvpart(data.matrix(dataAll[,1:ncol(y_train[[j]])])~.,
					data=as.data.frame(dataAll[,(ncol(y_train[[j]])+1):(ncol(dataAll))]),
              			xv="min",plot.add=FALSE)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
	}
	save(mrts, file=file.path(FD,set_no,paste("mrts_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_MRTS1_",dataN[sz],".RData",sep="")))
		rm(comTimes)
	}
	
	rm(mrts)
	rm(dataAll)
	gc()
}

##########################################################################################

