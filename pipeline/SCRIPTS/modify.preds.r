##########################################################################################
# OCCURRENCE PROBABILITIES
# resulting matrix: probabilties, nsites*nsp
##########################################################################################

sp_occ_probs <- rep( list(array(NA,dim=list(dim(y_valid[[1]])[1],dim(y_valid[[2]])[2],nmodels))), 3 ) 

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

    	sp_occ_probs[[j]][,,m] <- apply(pred_comms,c(1,2),mean)

		rm(pred_comms) 
		gc()   
	
		}	
	}
  	
  	save(sp_occ_probs, file=paste(RD2,Sets[d],"/sp_occ_probs_",dataN[sz],".RData",sep=""))
	
	rm(sp_occ_probs)   
	gc()   
	


##########################################################################################
# SPECIES RICHNESS AT SITE LEVEL
# resulting matrix: nsp, nsamples*nsites
##########################################################################################

sp_rich_site <- rep( list(array(NA,dim=list(dim(y_valid[[1]])[1],REPs,nmodels))), 3 ) 

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
	
		sp_rich_site[[j]][,,m] <- apply(pred_comms,3,rowSums,na.rm=T)
	
		rm(pred_comms) 
		gc()   
	
		}	
	}
  	
  	save(sp_rich_site, file=paste(RD2,Sets[d],"/sp_rich_site_",dataN[sz],".RData",sep=""))
	
	rm(sp_rich_site)   
	gc()   
	

##########################################################################################
# SPECIES RICHNESS AT AREA LEVEL
# resulting matrix: nsp, nsamples*nareas
##########################################################################################

sp_rich_area <- rep( list(array(NA,dim=list((nrow(s_valid[[j]])/10),REPs,nmodels))), 3 ) 
sp_occ_area <- rep( list(array(NA,dim=list((nrow(s_valid[[j]])/10),dim(y_valid[[1]])[2],REPs,nmodels))), 3 ) 

sp_occ_area_valid <- rep( list(matrix(NA,ncol=dim(y_valid[[1]])[2],nrow=(nrow(s_valid[[j]])/10))), 3 ) 
sp_rich_area_valid <- rep( list(matrix(NA,ncol=1,nrow=(nrow(s_valid[[j]])/10))), 3 ) 

for (j in 1:3) {		

	nsites <- nrow(s_valid[[j]])
	nsp <- ncol(y_valid[[j]])
	clusts <- kmeans(s_valid[[j]], round(nsites/10))$cluster
	svalid <- cbind(s_valid[[j]], clusts)
	nareas <- length(unique(clusts))

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
				sp_occ_area[[j]][s,,,m]<-(pred_comms[sel,,]>0)*1
				sp_rich_area[[j]][s,,m]<-colSums(pred_comms[sel,,])
				if (m==nmodels) {				
					sp_occ_area_valid[[j]][s,]<-y_valid[[j]][sel,]
					sp_rich_area_valid[[j]][s,]<-sum(y_valid[[j]][sel,])
				}
			}else{
				sp_occ_area[[j]][s,,,m]<-(apply(pred_comms[sel,,],3,colSums)>0)*1
				sp_rich_area[[j]][s,,m]<-colSums(apply(pred_comms[sel,,],3,colSums)>0)
				if (m==nmodels) {				
					sp_occ_area_valid[[j]][s,]<-(colSums(y_valid[[j]][sel,])>0)*1
					sp_rich_area_valid[[j]][s,]<-sum((colSums(y_valid[[j]][sel,])>0)*1)
				}
				}
			}
	
			rm(pred_comms) 
			gc()   
	
		}		
	}
  	
  	save(sp_occ_area, file=paste(RD2,Sets[d],"/sp_occ_area_",dataN[sz],".RData",sep=""))
  	save(sp_rich_area, file=paste(RD2,Sets[d],"/sp_rich_area_",dataN[sz],".RData",sep=""))

  	save(sp_occ_area_valid, file=paste(RD2,Sets[d],"/sp_occ_area_valid_",dataN[sz],".RData",sep=""))
  	save(sp_rich_area_valid, file=paste(RD2,Sets[d],"/sp_rich_area_valid_",dataN[sz],".RData",sep=""))
	
	rm(sp_occ_area)   
	rm(sp_rich_area)   
	rm(sp_occ_area_valid)   
	rm(sp_rich_area_valid)   
	gc()   
	
##########################################################################################
# COMMUNITY COMPOSITION
##########################################################################################

require(betapart)
# beta.multi(x, index.family="sorensen")

N<-as.numeric(dataN[sz])
beta_inds_site <- rep( list(array(NA,dim=list(N,3,REPs,nmodels))), 3 ) 
beta_inds_site_valid <- rep( list(matrix(NA,ncol=3,nrow=N)), 3 ) 

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

		sitecombs<-combn(1:N, 2)
		set.seed(7)
		selsites<-sitecombs[,sample(1:ncol(sitecombs),N)]

		for (n in 1:REPs) {
			tmp1<-list()			
			tmp2<-list()			

			for (i in 1:N) {
				tmp1[[i]]<-cbind(pred_comms[selsites[1,i],,n],pred_comms[selsites[1,i],,n])
				if (m==nmodels & n==REPs) {				
					tmp2[[i]]<-cbind(y_valid[[j]][selsites[1,i],],y_valid[[j]][selsites[1,i],])
				}
			}
			beta_inds_site[[j]][,,n,m]<-matrix(unlist(lapply(tmp1,beta.multi)),ncol=3)
			if (m==nmodels & n==REPs) {				
				beta_inds_site_valid[[j]]<-matrix(unlist(lapply(tmp2,beta.multi)),ncol=3)
			}
		}
		rm(pred_comms) 
		gc()   
	}
}
	
  	save(beta_inds_site, file=paste(RD2,Sets[d],"/beta_inds_site_",dataN[sz],".RData",sep=""))
  	save(beta_inds_site_valid, file=paste(RD2,Sets[d],"/beta_inds_site_valid_",dataN[sz],".RData",sep=""))
	rm(beta_inds_site)   
	gc()   


##########################################################################################
