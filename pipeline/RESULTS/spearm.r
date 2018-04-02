##########################################################################################
# SPEARMAN CORRELATIONS
##########################################################################################
ensmblModels <- opts$modelEnsemble
prevThrs <- opts$prevaleceThreshold

filebody<-paste(RD2,Sets[d],"/beta_inds_site_valid_",sep="")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
load(file=paste(filebody,dataN[sz],".RData",sep=""))

filesnames<-c("sp_rich_site_","beta_inds_site_")

filebodies<-paste(RD2,Sets[d],"/",filesnames,sep="")
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
	spRichSiteSpear <- matrix(NA,REPs,3)
	betaIndSpear <- rep( list(matrix(NA,REPs,3)), 3 )
} else {
	spRichSiteSpear <- array(NA, dim=list(nmodels,REPs,3))
	betaIndSpear <- rep( list(array(NA, dim=list(nmodels,REPs,3))), 3 )
}

for (j in 1:3) {

	if (is.numeric(prevThrs)) {
		sps<-which((colSums(y_valid[[j]])/nrow(y_valid[[j]]))>=prevThrs)
	} else {
		sps<-1:ncol(y_valid[[j]])
	}

	if (is.null(ensmblModels)!=TRUE) {
		spRichSiteSpear[,j] <- t(apply(sp_rich_site[[j]],2,cor,y=rowSums(y_valid[[j]][,sps]),method="spearman"))
		for (b in 1:3) {
			betaIndSpear[[b]][,j] <- t(apply(beta_inds_site[[j]][,b,],2,cor,y=beta_inds_site_valid[[j]][,b],method="spearman",use="complete.obs"))
		}
	} else {
		spRichSiteSpear[,,j] <- t(apply(sp_rich_site[[j]],3,apply,2,cor,y=rowSums(y_valid[[j]][,sps]),method="spearman"))
		for (b in 1:3) {
			betaIndSpear[[b]][,,j]	<- t(apply(beta_inds_site[[j]][,b,,],3,apply,2,cor,y=beta_inds_site_valid[[j]][,b],method="spearman",use="complete.obs"))
		}
	}

}

filebody1<-paste(RDfinal,dataN[sz],"/spRichSiteSpear_",sep="")
filebody2<-paste(RDfinal,dataN[sz],"/betaIndSpear_",sep="")
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
save(spRichSiteSpear, file=paste(filebody1,Sets[d],".RData",sep=""))
save(betaIndSpear, file=paste(filebody2,Sets[d],".RData",sep=""))


if (sum(is.na(unlist(spRichSiteSpear)))>0){
	paste("spRichSiteSpear contains",sum(is.na(unlist(betaIndSpear))),"NA values")
}
if (sum(is.na(unlist(betaIndSpear)))>0){
	paste("betaIndSpear contains",sum(is.na(unlist(betaIndSpear))),"NA values")
}

##########################################################################################

if (is.null(ensmblModels)!=TRUE) {
	tmp1<-colMeans(spRichSiteSpear,na.rm=T)
	tmp3<-lapply(betaIndSpear,colMeans,na.rm=T)
} else {
	tmp1<-apply(spRichSiteSpear,3,rowMeans,na.rm=T)
	tmp3<-list()
	for (b in 1:3) {
		tmp3[[b]]<-apply(betaIndSpear[[b]],3,rowMeans,na.rm=T)
	}
}

PMs[[9]]<-as.vector(tmp1)
names(PMs)[9]<-"discrimination2site"

for (b in 1:3) {
	PMs[[(9+b)]]<-as.vector(tmp3[[b]])
	names(PMs)[(9+b)]<-paste("discrimination3beta",b,sep="")
}
rm(tmp1,tmp3)
gc()

##########################################################################################

