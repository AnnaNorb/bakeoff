##########################################################################################
# STANDARD DEVIATIONS
##########################################################################################
ensmblModels <- opts$modelEnsemble
prevThrs <- opts$prevaleceThreshold

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
	spRichSiteSD <- matrix(NA, dim(sp_rich_site[[1]])[1],3)
	betaIndSD <- rep( list(matrix(NA,dim(beta_inds_site[[1]])[1],3)), 3 )
} else {
	spRichSiteSD <- array(NA, dim=list(nmodels,dim(sp_rich_site[[1]])[1],3))
	betaIndSD <- rep( list(array(NA, dim=list(nmodels,dim(beta_inds_site[[1]])[1],3))), 3 )
}

for (j in 1:3) {
	if (is.null(ensmblModels)!=TRUE) {
		spRichSiteSD[,j] <- apply(sp_rich_site[[j]],1,sd)
		for (b in 1:3) {
			betaIndSD[[b]][,j] <- apply(beta_inds_site[[j]][,b,],1,sd)
		}
	} else {
		for (m in 1:nmodels) {

			spRichSiteSD[m,,j] <- apply(sp_rich_site[[j]][,,m],1,sd)

			for (b in 1:3) {
				betaIndSD[[b]][m,,j] <- apply(beta_inds_site[[j]][,b,,m],1,sd)
				}
		}
	}
}

filebody1<-file.path(RDfinal,dataN[sz],"/spRichSiteSD_")
filebody2<-file.path(RDfinal,dataN[sz],"/betaIndSD_")
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
save(spRichSiteSD, file=paste(filebody1,Sets[d],".RData",sep=""))
save(betaIndSD, file=paste(filebody2,Sets[d],".RData",sep=""))

##########################################################################################

# if (is.null(ensmblModels)!=TRUE) {
# 	tmp1<-colMeans(spRichSiteSD)
# 	tmp3<-lapply(betaIndSD,colMeans)
# } else {
# 	tmp1<-apply(spRichSiteSD,3,rowMeans)
# 	tmp3<-list()
# 	for (b in 1:3) {
# 		tmp3[[b]]<-apply(betaIndSD[[b]],3,rowMeans)
# 	}
# }
# 
# PMs[[13]]<-as.vector(tmp1)
# names(PMs)[13]<-"sharpness2site"
# 
# for (b in 1:3) {
# 	PMs[[(13+b)]]<-as.vector(tmp3[[b]])
# 	names(PMs)[(13+b)]<-paste("sharpness3beta",b,sep="")
# }
# rm(tmp1,tmp3)
# gc()

##########################################################################################
