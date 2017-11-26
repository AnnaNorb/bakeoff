##########################################################################################
# STATISTICAL METHODS FOR ANALYSING MULTIVARIATE ABUNDANCE DATA PREDICTIONS
##########################################################################################

require(mvabund)

##########################################################################################

for (j in 1:3) {

	load(file=paste(FD,set_no,"/traitglm1_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/traitglm2_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/traitglm3_",j,"_",dataN[sz],".RData",sep=""))

	Xv <- data.frame(x_valid[[j]][,-1])
	Xvs <- data.frame(cbind(x_valid[[j]][,-1],s_valid[[j]]))
	Yv <- mvabund(y_valid[[j]])

	nsp <- ncol(Yv)
	nsites <- nrow(Xv)

	traitglm1$data<-Xv
	traitglm2$data<-Xvs
	traitglm3$data<-Xv

	traitglm1$family<-binomial(link = "logit")
	traitglm2$family<-binomial(link = "logit")
	traitglm3$family<-binomial(link = "logit")

	traitglm1_odds <- predict(traitglm1,newdata=Xv)
	traitglm2_odds <- predict(traitglm2,newdata=Xvs)
	traitglm3_odds <- predict(traitglm3,newdata=Xv)

	traitglm_PAs <- list()
	traitglm2_PAs <- list()
	traitglm3_PAs <- list()
	Z1 <- array(NA, dim=list(nsites,nsp,REPs))
	Z2 <- Z1
	Z3 <- Z1
	for (n in 1:REPs) {
		Z1[,,n] <- unlist(rnorm(traitglm1_odds,mean=traitglm1_odds,sd=1))
		Z2[,,n] <- unlist(rnorm(traitglm2_odds,mean=traitglm1_odds,sd=1))
		Z3[,,n] <- unlist(rnorm(traitglm3_odds,mean=traitglm1_odds,sd=1))
		}
		
	traitglm1_PAs <- (Z1>0)*1
	traitglm2_PAs <- (Z2>0)*1
	traitglm3_PAs <- (Z3>0)*1

	save(traitglm1_PAs, file=paste(PD2,set_no,"/traitglm1_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(traitglm2_PAs, file=paste(PD2,set_no,"/traitglm2_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(traitglm3_PAs, file=paste(PD2,set_no,"/traitglm3_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(traitglm1_PAs)
	rm(traitglm2_PAs)
	rm(traitglm3_PAs)
	rm(Z1)
	rm(Z2)
	rm(Z3)
	rm(traitglm1_odds)
	rm(traitglm2_odds)
	rm(traitglm3_odds)
	rm(traitglm1)
	rm(traitglm2)
	rm(traitglm3)
	gc()
	
	}
##########################################################################################
	