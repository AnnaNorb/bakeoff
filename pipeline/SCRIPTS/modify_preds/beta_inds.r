##########################################################################################
# COMMUNITY COMPOSITION
##########################################################################################
require(abind)
require(betapart)
# beta.multi(x, index.family="sorensen")

ensmblModels <- opts$modelEnsemble
prevThrs <- opts$prevaleceThreshold

n1<-600
n2<-300
#beta_inds_site <- rep( list( array(NA,dim=list(n2,3,REPs,nmodels)) ),3 ) 
beta_inds_site <- list()
beta_inds_site_valid <- list()

sitecombs<-combn(1:n1, 2)
set.seed(7)
selsites<-sitecombs[,sample(1:ncol(sitecombs),n2,replace=T)]

for (j in 1:3) {

	if (is.numeric(prevThrs)) {
		sps<-which((colSums(y_valid[[j]])/nrow(y_valid[[j]]))>=prevThrs)
	} else {
		sps<-1:ncol(y_valid[[j]])
	}

	if (is.null(ensmblModels)!=TRUE) {
		pred_names_tmp<-pred_names[ensmblModels]
		pred_comms_ensemble<-array(NA,dim=list(dim(y_valid[[j]][,sps])[1],dim(y_valid[[j]][,sps])[2],1))
		betaIndsSite <- array(NA,dim=list(n2,3,REPs))
	} else {
		pred_names_tmp<-pred_names
		betaIndsSite <- array(NA,dim=list(n2,3,REPs,nmodels))
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
			for (n in 1:REPs) {
				tmp1<-list()			
				tmp2<-list()			

				for (i in 1:n2) {
					tmp1[[i]]<-rbind(pred_comms[selsites[1,i],sps,n],pred_comms[selsites[2,i],sps,n])
					#if (m==nmodels & n==REPs) {				
						tmp2[[i]]<-rbind(y_valid[[j]][selsites[1,i],sps],y_valid[[j]][selsites[2,i],sps])
					#}
				}
				tmp3<-matrix(unlist(lapply(tmp1,beta.pair)),ncol=3,byrow=T)
					tmp3[which(is.nan(tmp3[,1])),1]<-1
					tmp3[which(is.nan(tmp3[,2])),2]<-0
					tmp3[which(is.nan(tmp3[,3])),3]<-1
				betaIndsSite[,,n,m]<-tmp3
				if (m==length(pred_names_tmp) & n==REPs) {				
					tmp4<-matrix(unlist(lapply(tmp2,beta.pair)),ncol=3,byrow=T)
					tmp4[which(is.nan(tmp4[,1])),1]<-1
					tmp4[which(is.nan(tmp4[,2])),2]<-0
					tmp4[which(is.nan(tmp4[,3])),3]<-1
					beta_inds_site_valid[[j]] <- tmp4
				}
			}
		}

		rm(pred_comms) 
		gc()   
	}

	if (is.null(ensmblModels)!=TRUE) {
		preds <- pred_comms_ensemble[,,-1]

		for (n in 1:REPs) {
			tmp1<-list()			

			for (i in 1:n2) {
				tmp1[[i]]<-rbind(preds[selsites[1,i],,n],preds[selsites[2,i],,n])
			}
			tmp3<-matrix(unlist(lapply(tmp1,beta.pair)),ncol=3,byrow=T)
				tmp3[which(is.nan(tmp3[,1])),1]<-1
				tmp3[which(is.nan(tmp3[,2])),2]<-0
				tmp3[which(is.nan(tmp3[,3])),3]<-1
				betaIndsSite[,,n]<-tmp3
			rm(tmp1)
			rm(tmp3)
		}	
	rm(pred_comms_ensemble)
	}
	beta_inds_site[[j]]	<-	betaIndsSite
}

filebody<-paste(RD2,Sets[d],"/beta_inds_site_",sep="")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
if (is.null(ensmblModels)!=TRUE) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
}
save(beta_inds_site, file=paste(filebody,dataN[sz],".RData",sep=""))
rm(beta_inds_site)   

if (is.null(ensmblModels)) {
	filebody<-paste(RD2,Sets[d],"/beta_inds_site_valid_",sep="")
	if (is.numeric(prevThrs)) {
		filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
	}
	save(beta_inds_site_valid, file=paste(filebody,dataN[sz],".RData",sep=""))
	rm(beta_inds_site_valid)   
}

gc()   


##########################################################################################

# tmpXerit<-matrix(c(c(0,1,0,1,1),c(1,0,1,0,0)),nrow=2,byrow=T)
# tmpXsamat<-matrix(c(c(0,1,0,1,1),c(0,1,0,1,1)),nrow=2,byrow=T)
# beta.pair(tmpXerit)
# beta.pair(tmpXsamat)