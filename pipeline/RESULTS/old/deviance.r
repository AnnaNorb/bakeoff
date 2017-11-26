##########################################################################################
# DEVIANCES
##########################################################################################

DEVS <- matrix(NA,nrow=nmodels,ncol=3)

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

	rm(pred_comms) 
	gc()   

	# muuta alla oleva niin, että 0 = 0.001, 1 = 0.999
	PROBs[which(PROBs==0,arr.ind=TRUE)]<-PROBs[which(PROBs==0,arr.ind=TRUE)]+abs(rnorm(length(PROBs[which(PROBs==0,arr.ind=TRUE)]),0,0.0001))
	PROBs[which(PROBs==1,arr.ind=TRUE)]<-PROBs[which(PROBs==1,arr.ind=TRUE)]-abs(rnorm(length(PROBs[which(PROBs==1,arr.ind=TRUE)]),0,0.0001))

	# muuta niin, että lasket keskiarvot noista (lajit * sampling units)
	DEVS[m,j] <- sum(-2*(y_valid[[j]]*log(PROBs)+(1-y_valid[[j]])*log(1-PROBs)),na.rm=T)

	rm(PROBs)   
	gc()   
	
		}	
	}
	
	save(DEVS, file=paste(RDfinal,dataN[sz],"/DEVS_",Sets[d],".RData",sep=""))


##########################################################################################


