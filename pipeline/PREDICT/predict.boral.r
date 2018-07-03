##########################################################################################
# BAYESIAN ORDINATIO AND REGRESSION ANALYSIS PREDICTIONS
##########################################################################################

require(boral)
source(file.path(PD,"boralPredict.r"))
require(abind)

##########################################################################################

for (j in 1:3) {

	nsites <- nrow(x_valid[[j]])
	nsp <- ncol(y_valid[[j]])
	#newIDs <- matrix(sample(1:300,600,replace=T),ncol=1)

	for (m in 1:2) {

		load(file=file.path(FD,set_no,paste("brl",m,"_",j,"_",dataN[sz],".RData",sep="")))
		if (m==1) { brl<-brl1 }
		if (m==2) { brl<-brl2 }

		Xv <- x_valid[[j]][,-1] 

		linpred_boral <- boralPredict(brl, newX = Xv, predict.type="marginal")
		boral_PAs <- linpred_boral
		for(k in 1:dim(linpred_boral)[3]) {
		    boral_PAs[,,k] <- matrix(rbinom(length(linpred_boral[,,k]), 1, prob = pnorm(linpred_boral[,,k])), nrow = dim(linpred_boral)[1], ncol = dim(linpred_boral)[2])
		}
		set.seed(7)
		smplREPs <- sample(1:dim(boral_PAs)[3], REPs, replace=T)
		boral_PAs <- boral_PAs[,,smplREPs]

# 		if (m!=2 & dataN[sz]!=600) {
# 			brl_preds <- boralPredict(brl, newX=Xv[1:dataN[sz],], predict.type="marginal")$all.linpred
# 			for (o in 2:(600/dataN[sz])) {
# 				tmp <- boralPredict(brl, newX=Xv[(dataN[sz]*(o-1)+1):(dataN[sz]*o),], predict.type="marginal")$all.linpred
# 				brl_preds <- abind(brl_preds,tmp,along=1)
# 			}
# 		} else if (m==2 & dataN[sz]==600) {
# 			brl_preds <- boralPredict(brl, newX=Xv, predict.type="marginal")$all.linpred
# 		} else if (m==2) {
# 			brl_preds <- boralPredict(brl, newX=Xv, predict.type="marginal", lv.mc=100)$all.linpred
# 		}	
# 		set.seed(7)
# 		samp<-sample(1:dim(brl_preds)[3],100,replace=T)
# 		brl_preds<-brl_preds[,,samp]
# 
# 		z <- rnorm(brl_preds,mean=brl_preds,sd=1)
# 		Z <- array(z,dim=list(dim(x_valid[[j]])[1],dim(y_valid[[j]])[2],REPs))
# 		boral_PAs <- (Z>0)*1
			
		save(boral_PAs, file=file.path(PD2,set_no,paste("boral",m,"_PAs_",j,"_",dataN[sz],".RData",sep="")))
			
		rm(brl)
		rm(linpred_boral)
		rm(resp_boral)
		rm(boral_PAs)
		gc()

	}
}	

##########################################################################################
