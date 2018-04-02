##########################################################################################
# SPECIES RICHNESS AT AREA LEVEL
# resulting matrix: nsp, nsamples*nareas
##########################################################################################

sp_occ_area <- list()
sp_rich_area <- list()

sp_occ_area_valid <- list()
sp_rich_area_valid <- list()

for (j in 1:3) {	

	if (is.numeric(prevThrs)) {
		sps<-which((colSums(y_valid[[j]])/nrow(y_valid[[j]]))>=prevThrs)
	} else {
		sps<-1:ncol(y_valid[[j]])
	}	

	nsites <- nrow(s_valid[[j]])
	nsp <- ncol(y_valid[[j]][,sps])
	clusts <- kmeans(s_valid[[j]], round(nsites/10))$cluster
	svalid <- cbind(s_valid[[j]], clusts)
	nareas <- length(unique(clusts))

	spRichArea <- array(NA,dim=list((nsites/10),REPs,nmodels))
	spOccArea <- array(NA,dim=list((nsites/10),nsp,REPs,nmodels))

	spOccAreaValid <- matrix(NA,ncol=nsp,nrow=nsites/10)
	spRichAreaValid <- matrix(NA,ncol=1,nrow=(nsites/10))

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

		for (s in 1:nareas) {
			ss<-as.numeric(unique(svalid[,ncol(svalid)]))[s]
			sel<-which(svalid[,ncol(svalid)]==ss,arr.ind=T)
			if (length(sel)==1){
				spOccArea[s,,,m]<-(pred_comms[sel,sps,]>0)*1
				spRichArea[s,,m]<-colSums(pred_comms[sel,sps,])
				if (m==nmodels) {				
					spOccAreaValid[s,]<-y_valid[[j]][sel,sps]
					spRichAreaValid[s,]<-sum(y_valid[[j]][sel,sps])
				}
			}else{

				spOccArea[s,,,m]<-(apply(pred_comms[sel,sps,],3,colSums)>0)*1
				spRichArea[s,,m]<-colSums(apply(pred_comms[sel,sps,],3,colSums)>0)
				if (m==nmodels) {				
					spOccAreaValid[s,]<-(colSums(y_valid[[j]][sel,sps])>0)*1
					spRichAreaValid[s,]<-sum((colSums(y_valid[[j]][sel,sps])>0)*1)
				}
			}
		}
		rm(pred_comms) 
	}		
	sp_occ_area[[j]] <- spOccArea
	sp_rich_area[[j]] <- spRichArea

	sp_occ_area_valid[[j]] <- spOccAreaValid
	sp_rich_area_valid[[j]] <- spRichAreaValid

	rm(spOccArea) 
	rm(spRichArea) 
	rm(spOccAreaValid) 
	rm(spRichAreaValid) 
	gc()   
}

if (is.numeric(prevThrs)) {
  	save(sp_occ_area, file=paste(RD2,Sets[d],"/sp_occ_area_spThr",prevThrs,"_",dataN[sz],".RData",sep=""))
  	save(sp_rich_area, file=paste(RD2,Sets[d],"/sp_rich_area_spThr",prevThrs,"_",dataN[sz],".RData",sep=""))

  	save(sp_occ_area_valid, file=paste(RD2,Sets[d],"/sp_occ_area_valid_spThr",prevThrs,"_",dataN[sz],".RData",sep=""))
  	save(sp_rich_area_valid, file=paste(RD2,Sets[d],"/sp_rich_area_valid_spThr",prevThrs,"_",dataN[sz],".RData",sep=""))
} else {
  	save(sp_occ_area, file=paste(RD2,Sets[d],"/sp_occ_area_",dataN[sz],".RData",sep=""))
  	save(sp_rich_area, file=paste(RD2,Sets[d],"/sp_rich_area_",dataN[sz],".RData",sep=""))

  	save(sp_occ_area_valid, file=paste(RD2,Sets[d],"/sp_occ_area_valid_",dataN[sz],".RData",sep=""))
  	save(sp_rich_area_valid, file=paste(RD2,Sets[d],"/sp_rich_area_valid_",dataN[sz],".RData",sep=""))
}

rm(sp_occ_area)   
rm(sp_rich_area)   
rm(sp_occ_area_valid)   
rm(sp_rich_area_valid)   
gc()   
	
##########################################################################################