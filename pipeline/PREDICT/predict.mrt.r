##########################################################################################
# MULTIVARIATE REGRESSION TREE
##########################################################################################

require(mvpart)
require(caret)

##########################################################################################

for (j in 1:3) {

	nsp <- length(DD_t[[j]])

	load(file=paste(FD,set_no,"/mrts_",j,"_",dataN[sz],".RData",sep=""))

	Yv <- y_valid[[j]]
	
	Xv <- x_valid[[j]]
	colnames(Xv)<-paste("V",(ncol(Yv)+1):(ncol(Yv)+ncol(Xv)),sep="")
	rownames(Xv)<-paste("e",1:nrow(Xv),sep="")
	Xv <- as.data.frame(Xv[,-1])

	mrt_probs <- predict(mrts,type="matrix", newdata=Xv)
	rm(mrts)

	nsp <- ncol(y_valid[[j]])
	nsites <- nrow(y_valid[[j]])

	mrt_PAs <- array(dim=c(nsites,nsp,REPs))

	for (n in 1:REPs){
  		mrt_PAs[,,n] <- rbinom(mrt_probs,1,mrt_probs)
		}
	rm(mrt_probs)

	save(mrt_PAs, file=paste(PD2,set_no,"/mrt_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(mrt_PAs)	
	gc()

	}
########################################################################################## 
