##########################################################################################
# 1a ACCURACY (|E[Y]-Y|)
##########################################################################################
ensmblModels <- opts$modelEnsemble
prevThrs <- opts$prevaleceThreshold

filebody<-paste(RD2,Sets[d],"/sp_occ_probs_",sep="")
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
	expctME <- matrix(NA,1,3)
} else {
	expctME <- matrix(NA,nmodels,3)
}


for (j in 1:3) {

	if (is.numeric(prevThrs)) {
		sps<-which((colSums(y_valid[[j]])/nrow(y_valid[[j]]))>=prevThrs)
	} else {
		sps<-1:ncol(y_valid[[j]])
	}

	if (is.null(ensmblModels)!=TRUE) {
			expctME[,j]<-mean(abs(sp_occ_probs[[j]]-y_valid[[j]][,sps]))	
	} else {
		for (m in 1:nmodels) {
			expctME[m,j]<-mean(abs(sp_occ_probs[[j]][,,m]-y_valid[[j]][,sps]))
		}	
	}
}

filebody<-paste(RDfinal,dataN[sz],"/expctME_",sep="")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
if (is.null(ensmblModels)!=TRUE) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
}
save(expctME, file=paste(filebody,Sets[d],".RData",sep=""))

##########################################################################################

PMs[[1]]<-as.vector(expctME)
names(PMs)[1]<-"accuracy1"

##########################################################################################
