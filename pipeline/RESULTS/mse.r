##########################################################################################
# ROOT MEAN SQUARED ERROR
##########################################################################################
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
	spRichSiteRMSE <- matrix(NA, nrow=1, ncol=3)
	BetaRMSE <- rep( list(matrix(NA, nrow=1, ncol=3)), 3 ) 
} else {
	spRichSiteRMSE <- matrix(NA, nrow=nmodels, ncol=3)
	BetaRMSE <- rep( list(matrix(NA, nrow=nmodels, ncol=3)), 3 ) 
}

for (j in 1:3) {

	if (is.numeric(prevThrs)) {
		sps<-which((colSums(y_valid[[j]])/nrow(y_valid[[j]]))>=prevThrs)
	} else {
		sps<-1:ncol(y_valid[[j]])
	}
			
	if (is.null(ensmblModels)!=TRUE) {
		spRichSiteRMSE[,j] <- mean(sqrt((matrix(rep(rowSums(y_valid[[j]][,sps]),times=ncol(sp_rich_site[[j]])),ncol=ncol(sp_rich_site[[j]]))-sp_rich_site[[j]])^2))
		for (b in 1:3) {
			BetaRMSE[[b]][,j] <- mean(sqrt((matrix(rep(beta_inds_site_valid[[j]][,b],times=REPs),ncol=REPs)-beta_inds_site[[j]][,b,])^2),na.rm=T)
		}
	} else {
		for (m in 1:nmodels) {
			spRichSiteRMSE[m,j] <- mean(sqrt((matrix(rep(rowSums(y_valid[[j]][,sps]),times=REPs),ncol=REPs)-sp_rich_site[[j]][,,m])^2))
			for (b in 1:3) {
				BetaRMSE[[b]][m,j] <- mean(sqrt((matrix(rep(beta_inds_site_valid[[j]][,b],times=REPs),ncol=REPs)-beta_inds_site[[j]][,b,,m])^2),na.rm=T)
			}
		}
	}

}

filebody1<-file.path(RDfinal,dataN[sz],"spRichSiteRMSE_")
filebody2<-file.path(RDfinal,dataN[sz],"BetaRMSE_")
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
save(spRichSiteRMSE, file=paste(filebody1,Sets[d],".RData",sep=""))
save(BetaRMSE, file=paste(filebody2,Sets[d],".RData",sep=""))


##########################################################################################

# PMs[[5]]<-as.vector(spRichSiteRMSE)
# names(PMs)[5]<-"accuracy2site"
# 
# for (b in 1:3) {
# 	PMs[[(5+b)]]<-as.vector(BetaRMSE[[b]])
# 	names(PMs)[(5+b)]<-paste("accuracy3beta",b,sep="")
# }

##########################################################################################
