##########################################################################################
# STATISTICAL METHODS FOR ANALYSING MULTIVARIATE ABUNDANCE DATA PREDICTIONS
##########################################################################################

require(mvabund)

##########################################################################################

for (j in 1:3) {

	load(file=file.path(FD,set_no,paste("manyglm1_",j,"_",dataN[sz],".RData",sep="")))

	Xv <- data.frame(x_valid[[j]][,-1])
	Yv <- mvabund(y_valid[[j]])

	nsp <- ncol(Yv)
	nsites <- nrow(Xv)

# 	manyglm1$data<-Xv

	manyglm1_probs <- predict(manyglm1,newdata=Xv,type="response")

	tmp <- foreach (i=1:REPs) %dopar% { rbinom(manyglm1_probs,1,manyglm1_probs) }
	manyglm1_PAs <- simplify2array(lapply(tmp,matrix,nrow=nsites,ncol=nsp))
	rm(tmp)

	save(manyglm1_PAs, file=file.path(PD2,set_no,paste("manyglm1_PAs_",j,"_",dataN[sz],".RData",sep="")))

	rm(manyglm1_PAs)
	rm(manyglm1_probs)
	rm(manyglm1)
	gc()
	
	}
##########################################################################################
	