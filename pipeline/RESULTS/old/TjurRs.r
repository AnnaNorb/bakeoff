##########################################################################################
# TJUR R2'S
##########################################################################################

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

    PROBs <- apply(pred_comms,c(1,2),mean)

    tjurs_0 <- matrix(NA, ncol=ncol(y_valid[[j]]), nrow=nrow(y_valid[[j]]))    
    tjurs_0[which(y_valid[[j]]==0,arr.ind=TRUE)] <- PROBs[which(y_valid[[j]]==0,arr.ind=TRUE)]
    tjurs_0_means <- colMeans(tjurs_0, na.rm=TRUE)
    rm(tjurs_0)
    
    tjurs_1 <- matrix(NA, ncol=ncol(y_valid[[j]]), nrow=nrow(y_valid[[j]]))    
    tjurs_1[which(y_valid[[j]]==1,arr.ind=TRUE)] <- PROBs[which(y_valid[[j]]==1,arr.ind=TRUE)]
    tjurs_1_means <- colMeans(tjurs_1, na.rm=TRUE)
    rm(tjurs_1)
    
    tjurs <- tjurs_1_means-tjurs_0_means
    rm(tjurs_1_means)
    rm(tjurs_0_means)
    rm(PROBs)
    
  	save(tjurs, file=paste(RD2,"Tjurs/",Sets[d],"/tjurs_",mod_names[[m]],"_",j,"_",dataN[sz],".RData",sep=""))
  	
	rm(tjurs)
    gc()
    
    	}
	}
##########################################################################################
