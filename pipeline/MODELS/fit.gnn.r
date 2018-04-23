##########################################################################################
# GRADIENT NEAREST NEIGHBOUR  
##########################################################################################

require(yaImpute)
require(vegan)

##########################################################################################

for (j in 1:3) {

	nsp <- ncol(y_train[[j]])

	Xt <- as.data.frame(x_train[[j]][,-1])
	colnames(Xt)<-paste("V",1:ncol(Xt),sep="")
	rownames(Xt)<-paste("t",1:nrow(Xt),sep="")

	Yt <- as.matrix(y_train[[j]])
	rownames(Yt)<-paste("t",1:nrow(Yt),sep="")

	if (j==1) { sT<-Sys.time() }
	gnn1 <- yai(x=Xt,y=Yt,method="gnn",k=10)
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
	}
	save(gnn1, file=paste(FD,set_no,"/gnn1_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_GNN1_",dataN[sz],".RData",sep=""))
		rm(comTimes)
	}

	rm(Xt)
	rm(Yt)

}
##########################################################################################
