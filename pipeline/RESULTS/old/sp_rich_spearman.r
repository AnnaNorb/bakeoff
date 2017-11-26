##########################################################################################
# SPECIES RICHNESS SPEARMAN CORRELATIONS
##########################################################################################

spRichSpearm <- array(NA, dim=list(nmodels,REPs,3))

for (j in 1:3) {		

	for (m in models) {

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

		#if (sum(is.na(pred_comms_I))>0) { pred_comms_I[which(is.na(pred_comms_I),arr.ind=T)]<-round(colMeans(apply(pred_comms_I[,which(is.na(pred_comms_I),arr.ind=T)[,2],],c(1,2),mean),na.rm=T)) }
		if (length(unique(sprich))==1) { sprich+rnorm(sprich,mean=0,sd=0.0001) }

		spRichSpearm[m,,j] <- apply(sprich,1,cor,y=rowSums(y_valid[[j]]), method="spearman")

		}
	}

save(spRichSpearm, file=paste(RDfinal,dataN[sz],"/spRichSpearm_",Sets[d],".RData",sep=""))

##########################################################################################
