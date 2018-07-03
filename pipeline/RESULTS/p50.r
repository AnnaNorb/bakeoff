###########################################################################################
#|p-0.5|, where p is the PROPORTION OF PREDICTIONS THAT FALL WITHIN 50% PREDICTION INTERVAL
###########################################################################################
ensmblModels <- opts$modelEnsemble
prevThrs <- opts$prevaleceThreshold

filebody<-file.path(RD2,Sets[d],"beta_inds_site_valid_")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
load(file=paste(filebody,dataN[sz],".RData",sep=""))

filesnames<-c("sp_rich_site_","beta_inds_site_")

filebodies<-paste(file.path(RD2,Sets[d]),filesnames,sep="/")
if (is.numeric(prevThrs)) {
	filebodies<-paste(filebodies,"spThr",prevThrs*100,"_",sep="")
}
if (is.null(ensmblModels)!=TRUE) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebodies<-paste(filebodies,"ensmbl_",ensmbl,"_",sep="")
}
for (f in 1:length(filebodies)) {
	load(file=paste(filebodies[f],dataN[sz],".RData",sep=""))
}

if (is.null(ensmblModels)!=TRUE) {
	f50_sprichSite <- matrix(NA, nrow=1, ncol=3)
	f50_Betas <- rep( list(matrix(NA, nrow=1, ncol=3)), 3 ) 
} else {
	f50_sprichSite <- matrix(NA, nrow=nmodels, ncol=3)
	f50_Betas <- rep( list(matrix(NA, nrow=nmodels, ncol=3)), 3 ) 
}


for (j in 1:3) {	

	if (is.numeric(prevThrs)) {
		sps<-which((colSums(y_valid[[j]])/nrow(y_valid[[j]]))>=prevThrs)
	} else {
		sps<-1:ncol(y_valid[[j]])
	}	
	nsites<-300
	q50betas<-list()

	if (is.null(ensmblModels)!=TRUE) {
		inside50qSites <- matrix(NA,nrow=nsites,ncol=1)
		inside50qBetas <- matrix(NA,nsites,3)
		q50sites <- t(apply(sp_rich_site[[j]], 1, quantile, probs=c(0.25,0.75), na.rm=T, type=3))
		for (b in 1:3) {
			q50betas[[b]] <- t(apply(beta_inds_site[[j]][,b,], 1, quantile, probs=c(0.25,0.75), na.rm=T, type=3))
		}
		for (i in 1:nsites) {
			inside50qSites[i,]<-(rowSums(y_valid[[j]][,sps])[i]>=q50sites[i,1] & rowSums(y_valid[[j]][,sps])[i]<=q50sites[i,2])*1
			for (b in 1:3) {
				inside50qBetas[i,b] <- (beta_inds_site_valid[[j]][i,b]>=q50betas[[b]][i,1] & beta_inds_site_valid[[j]][i,b]<=q50betas[[b]][i,2])*1
			}
		}
	} else {
		inside50qSites <- matrix(NA,nrow=nsites,ncol=nmodels)
		inside50qBetas <- array(NA,dim=list(nsites,nmodels,3))
		
		for (m in 1:nmodels) {

		q50sites <- t(apply(sp_rich_site[[j]][,,m], 1, quantile, probs=c(0.25,0.75), na.rm=T, type=3))
			for (b in 1:3) {
				q50betas[[b]] <- t(apply(beta_inds_site[[j]][,b,,m], 1, quantile, probs=c(0.25,0.75), na.rm=T, type=3))
			}
			for (i in 1:nsites) {
				inside50qSites[i,m]<-(rowSums(y_valid[[j]][,sps])[i]>=q50sites[i,1] & rowSums(y_valid[[j]][,sps])[i]<=q50sites[i,2])*1
				for (b in 1:3) {
					inside50qBetas[i,m,b] <- (beta_inds_site_valid[[j]][i,b]>=q50betas[[b]][i,1] & beta_inds_site_valid[[j]][i,b]<=q50betas[[b]][i,2])*1
				}
			}
		}
	}
	
	f50_sprichSite[,j]<-apply(inside50qSites,2,sum)/nsites
	if (is.null(ensmblModels)!=TRUE) {
		for (b in 1:3) {
			f50_Betas[[b]][,j]<-sum(inside50qBetas[,b],na.rm=T)/nsites
		}
	} else {
		for (b in 1:3) {
			f50_Betas[[b]][,j]<-colSums(inside50qBetas[,,b],na.rm=T)/nsites
		}	
	}
}

filebody1<-file.path(RDfinal,dataN[sz],"/f50_sprichSite_")
filebody2<-file.path(RDfinal,dataN[sz],"/f50_Betas_")
if (is.numeric(prevThrs)) {
	filebody1<-paste(filebody1,"spThr",prevThrs*100,"_",sep="")
	filebody2<-paste(filebody2,"spThr",prevThrs*100,"_",sep="")
}
if (is.null(ensmblModels)!=TRUE) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebody1<-paste(filebody1,"ensmbl_",ensmbl,"_",sep="")
	filebody2<-paste(filebody2,"ensmbl_",ensmbl,"_",sep="")
}
save(f50_sprichSite, file=paste(filebody1,Sets[d],".RData",sep=""))
save(f50_Betas, file=paste(filebody2,Sets[d],".RData",sep=""))

##########################################################################################

# tmp1<-abs(0.5-f50_sprichSite)
# tmp3<-list()
# for (b in 1:3) {
# 	tmp3[[b]]<-abs(0.5-f50_Betas[[b]])
# }
# 
# PMs[[17]]<-as.vector(tmp1)
# names(PMs)[17]<-"calibration2site"
# 
# for (b in 1:3) {
# 	PMs[[(17+b)]]<-as.vector(tmp3[[b]])
# 	names(PMs)[(17+b)]<-paste("calibration3beta",b,sep="")
# }
# rm(tmp1,tmp3)
# gc()

##########################################################################################
