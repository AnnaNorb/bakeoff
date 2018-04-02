##########################################################################################
# OCCURRENCE PROBABILITIES
# resulting matrix: probabilties, nsites*nsp
##########################################################################################
require(abind)

ensmblModels <- opts$modelEnsemble
prevThrs <- opts$prevaleceThreshold

sp_occ_probs <- list() 

for (j in 1:3) {

	if (is.numeric(prevThrs)) {
		sps<-which((colSums(y_valid[[j]])/nrow(y_valid[[j]]))>=prevThrs)
	} else {
		sps<-1:ncol(y_valid[[j]])
	}

	if (is.null(ensmblModels)!=TRUE) {
		pred_names_tmp<-pred_names[ensmblModels]
		pred_comms_ensemble<-array(NA,dim=list(dim(y_valid[[j]][,sps])[1],dim(y_valid[[j]][,sps])[2],1))
	} else {
		pred_names_tmp<-pred_names
		spOccProb <- array(NA,dim=list(dim(y_valid[[j]][,sps])[1],dim(y_valid[[j]][,sps])[2],nmodels))
	}

	for (m in 1:length(pred_names_tmp)) {

    	model <- local({
    	load(paste(PD2, Sets[d],"/",pred_names_tmp[[m]],j,"_",dataN[sz],".RData",sep=""))
    	stopifnot(length(ls())==1)
    	environment()[[ls()]]
    	})

		pred_comms <- simplify2array(model)
		pred_comms <- as.array(pred_comms)
		if (any(is(pred_comms)== "simple_sparse_array")) {
			pred_comms<- array(pred_comms$v, dim=pred_comms$dim)
		}

		if (is.null(ensmblModels)!=TRUE) {
			predSamp<-sample(1:REPs,ceiling(REPs/length(ensmblModels)))	
			pred_comms_ensemble<-abind(pred_comms_ensemble,pred_comms[,sps,predSamp])
		} else {
			tmp <- apply(pred_comms[,sps,],c(1,2),mean)
    		spOccProb[,,m] <- (tmp*0.99)+0.005  	
			#To avoid singular cases due to the prediction being exactly 0 or 1, 
			#we transformed the probabilities by first multiplying them by 0.99 and then adding 0.005, 
			#so that they were in the range [0.005, 0.995] instead of the range [0,1]
			rm(tmp) 
		}
		rm(pred_comms) 
		gc()   
	}

	if (is.null(ensmblModels)!=TRUE) {
		tmp <- pred_comms_ensemble[,,-1]
		tmp <- apply(tmp,c(1,2),mean)
		spOccProb <- (tmp*0.99)+0.005  	
		rm(tmp)
	}		

	sp_occ_probs[[j]] <- spOccProb	
	rm(pred_names_tmp)
}

filebody<-paste(RD2,Sets[d],"/sp_occ_probs_",sep="")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
if (is.null(ensmblModels)!=TRUE) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
}
save(sp_occ_probs, file=paste(filebody,dataN[sz],".RData",sep=""))

rm(sp_occ_probs)   
gc()   

##########################################################################################