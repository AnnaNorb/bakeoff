##########################################################################################
# SPECIES RICHNESS CI errors
##########################################################################################

fractInSpRich95CIs <- matrix(NA, nrow=nmodels, ncol=3)

for (j in 1:3) {		
	insideSprich95CIs <- matrix(NA,nrow=nrow(y_valid[[j]]),ncol=nmodels)
	
	for (m in 1:nmodels) {

    	model <- local({
    	load(paste(PD2, Sets[d],"/",pred_names[[m]],j,"_",dataN[sz],".RData",sep=""))
    	stopifnot(length(ls())==1)
    	environment()[[ls()]]
    	})

	pred_comms <- simplify2array(model)
    pred_comms <- as.array(pred_comms)

	if (any(is(pred_comms)== "simple_sparse_array")) {
		pred_comms<- array(pred_comms$v, dim=pred_comms$dim)
	}

	nsites <- dim(pred_comms[,,1])[1]
	nsp <- dim(pred_comms[,,1])[2]

	sprich <- t(apply(pred_comms,3,rowSums,na.rm=T))
 
	# 0.025% and 0.975 quantiles of predicted species richnesses
	sprich95q <- t(apply(sprich, 2, quantile, probs=c(0.025,0.975), na.rm=T, type=3))

		for (i in 1:ncol(sprich)) {
			insideSprich95CIs[i,m]<-(rowSums(y_valid[[j]])[i]>=sprich95q[i,1] & rowSums(y_valid[[j]])[i]<=sprich95q[i,2])*1
			}
		}

	fractInSpRich95CIs[,j]<-colSums(insideSprich95CIs)/nsites
	
	}

# errors in CIs
spRichCI95err<-0.95-fractInSpRich95CIs

save(fractInSpRich95CIs, file=paste(RDfinal,dataN[sz],"/fractInSpRich95CIs_",Sets[d],".RData",sep=""))
save(spRichCI95err, file=paste(RDfinal,dataN[sz],"/spRichCI95err_",Sets[d],".RData",sep=""))

##########################################################################################
