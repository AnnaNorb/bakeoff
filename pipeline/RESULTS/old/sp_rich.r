##########################################################################################
# SPECIES RICHNESS & CIs
##########################################################################################

sprichCI50 <- matrix(NA,nrow=nmodels,ncol=3)
sprichCI95 <- matrix(NA,nrow=nmodels,ncol=3)
sprich_err2means <- matrix(NA,nrow=nmodels,ncol=3)
CI50err <- matrix(NA,nrow=nmodels,ncol=3)
CI95err <- matrix(NA,nrow=nmodels,ncol=3)

for (j in 1:3) {		

	realSprich50CIs<-matrix(NA,nrow=nrow(y_valid[[j]]),ncol=nmodels)
	realSprich95CIs<-matrix(NA,nrow=nrow(y_valid[[j]]),ncol=nmodels)

	for (m in models) {
    	model <- local({
    	load(paste(PD2, Sets[d],"/",pred_names[[m]],j,"_",dataN[sz],".RData",sep=""))
    	stopifnot(length(ls())==1)
    	environment()[[ls()]]
    	})

		pred_comms <- simplify2array(model)
		pred_comms <- as.array(pred_comms)

		if (is(pred_comms)== "simple_sparse_array") {
			pred_comms<- array(pred_comms$v, dim=pred_comms$dim)
		}
    
		nsites <- dim(pred_comms[,,1])[1]
		nsp <- dim(pred_comms[,,1])[2]

		sprich <- t(apply(pred_comms,3,rowSums,na.rm=T))
 
		# 0.25% and 0.75 quantiles of predicted species richnesses
		sprich50q <- t(apply(sprich, 2, quantile, probs=c(0.25,0.75), na.rm=T, type=3))

		# 0.05% and 0.95 quantiles of predicted species richnesses
		sprich95q <- t(apply(sprich, 2, quantile, probs=c(0.025,0.975), na.rm=T, type=3))

		# realized, accurate CIs
		for (i in 1:dim(sprich)[2]){
			realSprich50CIs[i,m] <- sum(sprich[,i]>=sprich50q[i,1] & sprich[,i]<=sprich50q[i,2])/REPs
			realSprich95CIs[i,m] <- sum(sprich[,i]>=sprich95q[i,1] & sprich[,i]<=sprich95q[i,2])/REPs
			}

		# no. cases within the 25% and 75% quantiles = 50% CI    
		sprich_50ci <- (sum(rowSums(y_valid[[j]])>=sprich50q[,1]&rowSums(y_valid[[j]])<=sprich50q[,2]))/nsites
		sprichCI50[m,j] <- sprich_50ci

		# no. cases within the 5% and 95% quantiles = 90% CI        
		sprich_95ci <- (sum(rowSums(y_valid[[j]])>=sprich95q[,1]&rowSums(y_valid[[j]])<=sprich95q[,2]))/nsites_i
		sprichCI95[m,j] <- sprich_95ci

		# errors in CIs
		CI50err[m,j]<-sprichCI50[m,j]-mean(realSprich50CIs[,m])
		CI95err[m,j]<-sprichCI95[m,j]-mean(realSprich95CIs[,m])
  
		# means squared errors over the sites
		sprich_err2means[m,j] <- mean((colMeans(sprich, na.rm=T)-rowSums(y_valid[[j]]))^2)
 
		}
	}

save(sprich_err2means, file=paste(RDfinal,dataN[sz],"/sprich_err2means_",Sets[d],".RData",sep=""))
save(CI50err, file=paste(RDfinal,dataN[sz],"/CI50err_",Sets[d],".RData",sep=""))
save(CI95err, file=paste(RDfinal,dataN[sz],"/CI95err_",Sets[d],".RData",sep=""))

##########################################################################################