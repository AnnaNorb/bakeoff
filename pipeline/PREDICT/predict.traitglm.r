##########################################################################################
# STATISTICAL METHODS FOR ANALYSING MULTIVARIATE ABUNDANCE DATA PREDICTIONS
##########################################################################################

require(mvabund)

##########################################################################################

for (j in 1:3) {

	load(file=file.path(FD,set_no,paste("traitglm1_",j,"_",dataN[sz],".RData",sep="")))

	Xv <- data.frame(x_valid[[j]][,-1])
	Yv <- mvabund(y_valid[[j]])

	nsp <- ncol(Yv)
	nsites <- nrow(Xv)

	traitglm1$data<-Xv

	traitglm1$family<-binomial(link = "logit")

	traitglm1_probs <- predict(traitglm1,newdata=Xv)

# 	Z1 <- array(NA, dim=list(nsites,nsp,REPs))
# 	for (n in 1:REPs) {
# 		Z1[,,n] <- unlist(rnorm(traitglm1_odds,mean=traitglm1_odds,sd=1))
# 		}
# 	traitglm1_PAs <- (Z1>0)*1

	tmp <- foreach (i=1:REPs) %dopar% { rbinom(traitglm1_probs,1,traitglm1_probs) }
	traitglm1_PAs <- simplify2array(lapply(tmp,matrix,nrow=nsites,ncol=nsp))

	save(traitglm1_PAs, file=file.path(PD2,set_no,paste("traitglm1_PAs_",j,"_",dataN[sz],".RData",sep="")))

	rm(traitglm1_PAs)
	rm(traitglm1_probs)
	rm(traitglm1)
	gc()
	
	}
##########################################################################################
	