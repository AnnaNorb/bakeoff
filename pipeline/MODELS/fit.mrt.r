##########################################################################################
# MULTIVARIATE REGRESSION TREE
##########################################################################################

require('mvpart')
require('caret')

##########################################################################################

for (j in 1:3) {

	nsp <- length(DD_t[[j]])

	dataAll<-as.data.frame(cbind(y_train[[j]],x_train[[j]]))
	colnames(dataAll)<-paste("V",1:ncol(dataAll),sep="")
	dataAllSpat<-as.data.frame(cbind(y_train[[j]],x_train[[j]],s_train[[j]]))
	colnames(dataAllSpat)<-paste("V",1:ncol(dataAllSpat),sep="")
	nsp <- ncol(y_train[[j]])
	nsites <- nrow(dataAll)


	if (j==1) { sT<-Sys.time() }
	mrts <- mvpart(data.matrix(dataAll[,1:ncol(y_train[[j]])])~.,
					data=as.data.frame(dataAll[,(ncol(y_train[[j]])+2):(ncol(dataAll))]),
              			xv="min",plot.add=FALSE)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mrts, file=paste(FD,set_no,"/mrts_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_MRTS1_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
	mrts2 <- mvpart(data.matrix(dataAllSpat[,1:ncol(y_train[[j]])])~.,
						data=as.data.frame(dataAllSpat[,(ncol(y_train[[j]])+2):(ncol(dataAllSpat))]),
              			xv="min",plot.add=FALSE)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(mrts2, file=paste(FD,set_no,"/mrts_spat_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_MRTS2_",dataN[sz],".RData",sep=""))
	}

}

##########################################################################################

