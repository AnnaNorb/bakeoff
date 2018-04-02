##########################################################################################
# SPECIES RICHNESS AT SITE LEVEL
# resulting matrix: nsp, nsamples*nsites
##########################################################################################
require(abind)

ensmblModels <- opts$modelEnsemble
prevThrs <- opts$prevaleceThreshold

sp_rich_site <- list()

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
		spRichSite <- array(NA,dim=list(dim(y_valid[[j]][,sps])[1],REPs,nmodels))
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
			spRichSite[,,m] <- apply(pred_comms[,sps,],3,rowSums,na.rm=T)
		}

		rm(pred_comms) 
		gc()   
	
	}	

	if (is.null(ensmblModels)!=TRUE) {
		tmp <- pred_comms_ensemble[,,-1]
		spRichSite <- apply(tmp,3,rowSums,na.rm=T)
		rm(pred_comms_ensemble)
		rm(tmp)
	}		

	sp_rich_site[[j]] <- spRichSite
	rm(spRichSite)
}

filebody<-paste(RD2,Sets[d],"/sp_rich_site_",sep="")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
if (length(ensmblModels)!=0) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
}
save(sp_rich_site, file=paste(filebody,dataN[sz],".RData",sep=""))


rm(sp_rich_site)   
gc()   

##########################################################################################
