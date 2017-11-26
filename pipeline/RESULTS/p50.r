##########################################################################################
#|p-0.5|, where p is the proportion of predictions that fall within 50% prediction interval
##########################################################################################

load(file=paste(RD2,Sets[d],"/sp_rich_site_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/sp_rich_area_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/sp_rich_area_valid_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/beta_inds_site_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/beta_inds_site_valid_",dataN[sz],".RData",sep=""))

f50_sprichSite <- matrix(NA, nrow=nmodels, ncol=3)
f50_sprichArea <- matrix(NA, nrow=nmodels, ncol=3)
f50_Betas <- rep( list(matrix(NA, nrow=nmodels, ncol=3)), 3 ) 

nsites<-as.numeric(dataN[sz])
nareas<-nrow(sp_rich_area_valid[[1]])

for (j in 1:3) {		
	
	inside50qSites <- matrix(NA,nrow=nsites,ncol=nmodels)
	inside50qAreas <- matrix(NA,nrow=nareas,ncol=nmodels)
	inside50qBetas <- array(NA,dim=list(nsites,nmodels,3))
	
	q50betas<-list()
	
	for (m in 1:nmodels) {

	q50sites <- t(apply(sp_rich_site[[j]][,,m], 1, quantile, probs=c(0.25,0.75), na.rm=T, type=3))
	q50areas <- t(apply(sp_rich_area[[j]][,,m], 1, quantile, probs=c(0.25,0.75), na.rm=T, type=3))
		for (b in 1:3) {
			q50betas[[b]] <- t(apply(beta_inds_site[[j]][,b,,m], 1, quantile, probs=c(0.25,0.75), na.rm=T, type=3))
		}
		for (i in 1:nsites) {
			inside50qSites[i,m]<-(rowSums(y_valid[[j]])[i]>=q50sites[i,1] & rowSums(y_valid[[j]])[i]<=q50sites[i,2])*1
			for (b in 1:3) {
				inside50qBetas[i,m,b] <- (beta_inds_site_valid[[j]][i,b]>=q50betas[[b]][i,1] & beta_inds_site_valid[[j]][i,b]<=q50betas[[b]][i,2])*1
				}
			}
		for (i in 1:nareas) {
			inside50qAreas[i,m]<-(sp_rich_area_valid[[j]][i,]>=q50areas[i,1] & sp_rich_area_valid[[j]][i,]<=q50areas[i,2])*1
			}
		}

	f50_sprichSite[,j]<-colSums(inside50qSites)/nsites
	for (b in 1:3) {
		f50_Betas[[b]][,j]<-colSums(inside50qBetas[,,b])/nsites
		}
	f50_sprichArea[,j]<-colSums(inside50qAreas)/nareas
	
	}

save(f50_sprichSite, file=paste(RDfinal,dataN[sz],"/f50_sprichSite_",Sets[d],".RData",sep=""))
save(f50_sprichArea, file=paste(RDfinal,dataN[sz],"/f50_sprichArea_",Sets[d],".RData",sep=""))
save(f50_Betas, file=paste(RDfinal,dataN[sz],"/f50_Betas_",Sets[d],".RData",sep=""))

##########################################################################################

