##########################################################################################
# STANDARD DEVIATIONS
##########################################################################################

load(file=paste(RD2,Sets[d],"/sp_rich_site_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/sp_rich_area_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/beta_inds_site_",dataN[sz],".RData",sep=""))

spRichSiteSD <- array(NA, dim=list(nmodels,dim(sp_rich_site[[1]])[1],3))
spRichAreaSD <- array(NA, dim=list(nmodels,dim(sp_rich_area[[1]])[1],3))
betaIndSD <- rep( list(array(NA, dim=list(nmodels,dim(beta_inds_site[[1]])[1],3))), 3 )

for (j in 1:3) {
	for (m in models) {

	spRichSiteSD[m,,j] <- apply(sp_rich_site[[j]][,,m],1,sd)
	spRichAreaSD[m,,j] <- apply(sp_rich_area[[j]][,,m],1,sd)
	for (b in 1:3) {
		betaIndSD[[b]][m,,j] <- apply(beta_inds_site[[j]][,b,,m],1,sd)
		}
	}
}
save(spRichSiteSD, file=paste(RDfinal,dataN[sz],"/spRichSiteSD_",Sets[d],".RData",sep=""))
save(spRichAreaSD, file=paste(RDfinal,dataN[sz],"/spRichAreaSD_",Sets[d],".RData",sep=""))
save(betaIndSD, file=paste(RDfinal,dataN[sz],"/betaIndSD_",Sets[d],".RData",sep=""))

##########################################################################################

