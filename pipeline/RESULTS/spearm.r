##########################################################################################
# SPEARMAN CORRELATIONS
##########################################################################################

load(file=paste(RD2,Sets[d],"/sp_rich_site_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/sp_rich_area_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/sp_occ_area_valid_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/sp_rich_area_valid_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/beta_inds_site_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/beta_inds_site_valid_",dataN[sz],".RData",sep=""))

spRichSiteSpear <- array(NA, dim=list(nmodels,REPs,3))
spRichAreaSpear <- array(NA, dim=list(nmodels,REPs,3))
betaIndSpear <- rep( list(array(NA, dim=list(nmodels,REPs,3))), 3 )

for (j in 1:3) {
	spRichSiteSpear[,,j] <- t(apply(sp_rich_site[[j]],3,apply,2,cor,y=rowSums(y_valid[[j]]),method="spearman"))
	spRichAreaSpear[,,j] <- t(apply(sp_rich_area[[j]],3,apply,2,cor,y=rowSums(sp_rich_area_valid[[j]]),method="spearman"))
	for (b in 1:3) {
		betaIndSpear[[b]][,,j]	<- t(apply(beta_inds_site[[j]][,b,,],3,apply,2,cor,y=beta_inds_site_valid[[j]][,b],method="spearman"))
		}
}

save(spRichSiteSpear, file=paste(RDfinal,dataN[sz],"/spRichSiteSpear_",Sets[d],".RData",sep=""))
save(spRichAreaSpear, file=paste(RDfinal,dataN[sz],"/spRichAreaSpear_",Sets[d],".RData",sep=""))
save(betaIndSpear, file=paste(RDfinal,dataN[sz],"/betaIndSpear_",Sets[d],".RData",sep=""))

##########################################################################################