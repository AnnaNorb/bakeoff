##########################################################################################
# GRADIENT NEAREST NEIGHBOUR  
##########################################################################################

require(yaImpute)
require(vegan)
require(pROC)

##########################################################################################

Ks<-c(1,5,10,30,100)
ncovar<-(ncol(x_train[[1]])-1)/2
nsp <- ncol(y_train[[1]])

for (j in 1:3) {

	Xt <- as.data.frame(x_train[[j]][,c(2:(ncovar+1))])
	colnames(Xt)<-paste("V",1:ncol(Xt),sep="")
	rownames(Xt)<-paste("t",1:nrow(Xt),sep="")

	Yt <- as.matrix(y_train[[j]])
	rownames(Yt)<-paste("t",1:nrow(Yt),sep="")

	gnns<-list()
	gnns_allProbs<-list()
	rocs<-c()
	if (j==1) { sT<-Sys.time() }
	for (k1 in 1:length(Ks)) {
		gnns[[k1]] <- yai(x=Xt,y=Yt,method="gnn",k=Ks[k1])
	}
	for (k1 in 1:length(Ks)) {
		gnns_probs <- as.matrix(predict(gnns[[k1]], k=Ks[k1], method="dstWeighted"))
		noPredSp <- setdiff(colnames(y_train[[j]]),colnames(gnns_probs))
		predSp <- intersect(colnames(y_train[[j]]),colnames(gnns_probs))
		gnns_allProbs[[k1]] <- matrix(NA,ncol=nsp,nrow=nsites)
		colnames(gnns_allProbs[[k1]])<-colnames(y_train[[j]])
		gnns_allProbs[[k1]][,predSp]<-gnns_probs[,predSp]
		gnns_allProbs[[k1]][,noPredSp]<-colMeans(as.matrix((y_train[[j]][,noPredSp])))
		rocs[k1]<-roc(predictor=as.vector(gnns_allProbs[[k1]]),response=as.vector(y_train[[j]]))$auc
	}	
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
	}
	gnn1<-gnns[[which(rocs==max(rocs))]]
	
	save(gnn1, file=file.path(FD,set_no,paste("gnn1_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_GNN1_",dataN[sz],".RData",sep="")))
		rm(comTimes)
	}

	rm(Xt)
	rm(Yt)
	rm(gnn1)
	rm(gnns)
	rm(gnns_probs)
	rm(gnns_allProbs)
	rm(rocs)
	gc()
}
##########################################################################################
