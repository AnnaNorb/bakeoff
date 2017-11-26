##########################################################################################
# GRADIENT NEAREST NEIGHBOUR  
##########################################################################################

require(yaImpute)
require(vegan)

##########################################################################################

for (j in 1:3) {

	nsp <- length(DD_t[[j]])

	Xt <- as.data.frame(x_train[[j]][,-1])
	colnames(Xt)<-paste("V",1:ncol(Xt),sep="")
	rownames(Xt)<-paste("t",1:nrow(Xt),sep="")
	Xts <- cbind(Xt,s_train[[j]])
	colnames(Xts)<-paste("V",1:ncol(Xts),sep="")
	rownames(Xts)<-paste("t",1:nrow(Xts),sep="")
	Yt <- as.matrix(y_train[[j]])
	rownames(Yt)<-paste("t",1:nrow(Yt),sep="")

	if (j==1) { sT<-Sys.time() }
	gnn <- yai(x=Xt,y=Yt,method="gnn",k=10)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(gnn, file=paste(FD,set_no,"/gnn_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GNN1_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
	gnn2 <- yai(x=Xts,y=Yt,method="gnn",k=10)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(gnn2, file=paste(FD,set_no,"/gnn2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GNN2_",dataN[sz],".RData",sep=""))
	}

}
##########################################################################################
