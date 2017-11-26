##########################################################################################
# GRADIENT NEAREST NEIGHBOUR PREDICTION
##########################################################################################

require(yaImpute)
require(vegan)

##########################################################################################

for (j in 1:3) {
	
	nsp <- ncol(y_train[[j]])
	nsites <- nrow(y_train[[j]])

	expl <- as.data.frame(x_train[[j]][,-1])
	colnames(expl)<-paste("V",1:ncol(expl),sep="")
	rownames(expl)<-paste("t",1:nrow(expl),sep="")

	Xv <- as.data.frame(x_valid[[j]][,-1])
	colnames(Xv)<-paste("V",1:ncol(Xv),sep="")
	rownames(Xv)<-paste("i",1:nrow(Xv),sep="")

	load(file=paste(FD,set_no,"/gnn_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/gnn2_",j,"_",dataN[sz],".RData",sep=""))

	gnn_probs <- as.matrix(predict(gnn, newdata=Xv, k=10, method="dstWeighted")[,1:nsp])
	gnn2_probs <- as.matrix(predict(gnn, newdata=Xv, k=10, method="dstWeighted")[,1:nsp])

	#probmat<-matrix(rep(colMeans(y_train[[j]]),times=nsites),ncol=nsp,byrow=T)
	#probarr<-array(rep(probmat,times=REPs),dim=list(nrow(probmat),ncol(probmat),REPs))

	tmp <- foreach (i=1:REPs) %dopar% { rbinom(gnn_probs,1,gnn_probs) }
	gnn_PAs <- simplify2array(lapply(tmp,matrix,nrow=nsites,ncol=nsp))
	rm(tmp)
	tmp <- foreach (i=1:REPs) %dopar% { rbinom(gnn2_probs,1,gnn_probs) }
	gnn2_PAs <- simplify2array(lapply(tmp,matrix,nrow=nsites,ncol=nsp))
	rm(tmp)

	#if (any(is.na(gnn_PAs_I))) { gnn_PAs_I[which(is.na(gnn_PAs_I),arr.ind=T)]<-probarr[which(is.na(gnn_PAs_I),arr.ind=T)] }

	rm(gnn)
	rm(gnn2)	
	rm(gnn_probs)
	rm(gnn2_probs)

	save(gnn_PAs, file=paste(PD2,set_no,"/gnn_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(gnn2_PAs, file=paste(PD2,set_no,"/gnn2_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(gnn_PAs)
	rm(gnn2_PAs)

	}
	
##########################################################################################