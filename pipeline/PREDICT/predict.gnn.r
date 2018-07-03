##########################################################################################
# GRADIENT NEAREST NEIGHBOUR PREDICTION
##########################################################################################

require(yaImpute)
require(vegan)

##########################################################################################

for (j in 1:3) {
	
	nsp <- ncol(y_train[[j]])
	nsites <- nrow(y_valid[[j]])

	expl <- as.data.frame(x_train[[j]][,-1])
	colnames(expl)<-paste("V",1:ncol(expl),sep="")
	rownames(expl)<-paste("t",1:nrow(expl),sep="")

	Xv <- as.data.frame(x_valid[[j]][,-1])
	colnames(Xv)<-paste("V",1:ncol(Xv),sep="")
	rownames(Xv)<-paste("i",1:nrow(Xv),sep="")

	load(file=file.path(FD,set_no,paste("gnn1_",j,"_",dataN[sz],".RData",sep="")))

	gnn1_probs <- as.matrix(predict(gnn1, newdata=Xv, k=10, method="dstWeighted"))

	noPredSp<-setdiff(colnames(y_train[[j]]),colnames(gnn1_probs))
	predSp<-intersect(colnames(y_train[[j]]),colnames(gnn1_probs))

	gnn1_allProbs <- matrix(NA,ncol=nsp,nrow=nsites)
	colnames(gnn1_allProbs)<-colnames(y_train[[j]])
	gnn1_allProbs[,predSp]<-gnn1_probs[,predSp]
	gnn1_allProbs[,noPredSp]<-colMeans(as.matrix((y_train[[j]][,noPredSp])))

	if (any(is.na(gnn1_allProbs))) {
		stop("Predicted probabilities contain NAs")
	}

	if (any(gnn1_allProbs==1)) {
		gnn1_allProbs<-(gnn1_allProbs*0.99)+0.005
	}

	tmp1 <- foreach (i=1:REPs) %dopar% { rbinom(gnn1_allProbs,1,gnn1_allProbs) }
	gnn1_PAs <- simplify2array(lapply(tmp1,matrix,nrow=nsites,ncol=nsp))
	rm(tmp1)

	save(gnn1_PAs, file=file.path(PD2,set_no,paste("gnn1_PAs_",j,"_",dataN[sz],".RData",sep="")))

	rm(Xv)
	rm(gnn1)
	rm(gnn1_probs)
	rm(gnn1_PAs)
	gc()
	
}
	
##########################################################################################