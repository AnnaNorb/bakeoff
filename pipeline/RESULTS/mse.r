##########################################################################################
# MEAN SQUARED ERROR
##########################################################################################

load(file=paste(RD2,Sets[d],"/sp_rich_site_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/sp_rich_area_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/sp_occ_area_valid_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/sp_rich_area_valid_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/beta_inds_site_",dataN[sz],".RData",sep=""))
load(file=paste(RD2,Sets[d],"/beta_inds_site_valid_",dataN[sz],".RData",sep=""))


spRichSiteMSE <- matrix(NA, nrow=nmodels, ncol=3)
spRichAreaMSE <- matrix(NA, nrow=nmodels, ncol=3)
BetaMSE <- rep( list(matrix(NA, nrow=nmodels, ncol=3)), 3 ) 

for (j in 1:3) {		
	
	for (m in 1:nmodels) {

		spRichSiteMSE[m,j] <- mean((matrix(rep(rowSums(y_valid[[j]]),times=REPs),ncol=REPs)-sp_rich_site[[j]][,,m])^2)
		spRichAreaMSE[m,j] <- mean((matrix(rep(sp_rich_area_valid[[j]],times=REPs),ncol=REPs)-sp_rich_area[[j]][,,m])^2)
		for (b in 1:3) {
			BetaMSE[[b]][m,j] <- mean((matrix(rep(beta_inds_site_valid[[j]][,b],times=REPs),ncol=REPs)-beta_inds_site[[j]][,b,,m])^2)
			}
		}
	}

save(spRichSiteMSE, file=paste(RDfinal,dataN[sz],"/spRichSiteMSE_",Sets[d],".RData",sep=""))
save(spRichAreaMSE, file=paste(RDfinal,dataN[sz],"/spRichAreaMSE_",Sets[d],".RData",sep=""))
save(BetaMSE, file=paste(RDfinal,dataN[sz],"/BetaMSE_",Sets[d],".RData",sep=""))

##########################################################################################
