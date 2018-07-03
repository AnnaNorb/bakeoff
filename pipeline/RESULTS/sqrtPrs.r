##########################################################################################
# 1c SHARPNESS sqrt(Pr(Y=1)*(1-Pr(Y=0)))
##########################################################################################
ensmblModels <- opts$modelEnsemble
prevThrs <- opts$prevaleceThreshold

filebody<-file.path(RD2,Sets[d],"sp_occ_probs_")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
if (is.null(ensmblModels)!=TRUE) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
}
load(file=paste(filebody,dataN[sz],".RData",sep=""))

if (is.null(ensmblModels)!=TRUE) {
	sqrt_pr <- matrix(NA,1,3)
} else {
	sqrt_pr <- matrix(NA,nmodels,3)
}

if (is.null(ensmblModels)!=TRUE) {
	for (j in 1:3) {	
		sqrt_pr[,j]<-mean(sqrt(sp_occ_probs[[j]]*(1-sp_occ_probs[[j]])))
	}
} else {
	for (j in 1:3) {	
		sqrt_pr[,j]<-apply(sqrt(sp_occ_probs[[j]]*(1-sp_occ_probs[[j]])),3,mean)
	}
}

filebody<-file.path(RDfinal,dataN[sz],"sqrt_pr_")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
if (is.null(ensmblModels)!=TRUE) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
}
save(sqrt_pr, file=paste(filebody,Sets[d],".RData",sep=""))

##########################################################################################

# PMs[[3]]<-as.vector(sqrt_pr)
# names(PMs)[3]<-"sharpness1"

##########################################################################################
