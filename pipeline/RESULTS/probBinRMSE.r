##########################################################################################
# 1c CALIBRATION: 	Mean error between predicted and observed numbers of occurrences 
# 					in 10 probability bins (based on quantiles), averaged over species
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
	probBinRMSE <- matrix(NA,1,3)
} else {
	probBinRMSE <- matrix(NA,nmodels,3)
}

for (j in 1:3) {

	if (is.numeric(prevThrs)) {
		sps<-which((colSums(y_valid[[j]])/nrow(y_valid[[j]]))>=prevThrs)
	} else {
		sps<-1:ncol(y_valid[[j]])
	}
	nsp<-ncol(y_valid[[j]][,sps])

	if (is.null(ensmblModels)!=TRUE) {
		errors<-NA
		for (i in 1:nsp) {
			tmp<-sp_occ_probs[[j]][,i]	
			bins<-quantile(tmp, probs=seq(0, 1, 0.1))
		
			err<-list()
			b1<-which(tmp>=bins[1]&tmp<bins[2])
			err[[1]]<-tmp[b1]-y_valid[[j]][,sps][b1,i]
			for (b in 2:(length(bins)-2)) {
				b1<-which(tmp>=bins[b]&tmp<bins[b+1])	
				err[[b]]<-(tmp[b1]-y_valid[[j]][,sps][b1,i])/length(b1)
			}
			b1<-which(tmp>=bins[length(bins)-1]&tmp<bins[length(bins)])	
			err[[length(bins)-1]]<-tmp[b1]-y_valid[[j]][,sps][b1,i]
		
			if (length(unlist(err))==0) {
				err<-(tmp<-y_valid[[j]][,sps][,i])/length(tmp)
			}			
			errors<-c(errors,mean(sqrt(unlist(err)^2)))
		}
		probBinRMSE[,j]<-mean(errors[-1])
	} else {
		for (m in 1:nmodels) {
			errors<-NA
			for (i in 1:nsp) {
				tmp<-sp_occ_probs[[j]][,i,m]	
				#tmp<-(sp_occ_probs[[j]][,i,m]*0.99)+0.005  
				bins<-quantile(tmp, probs=seq(0, 1, 0.1))
			
				err<-list()
				b1<-which(tmp>=bins[1]&tmp<bins[2])
				err[[1]]<-tmp[b1]-y_valid[[j]][,sps][b1,i]
				for (b in 2:(length(bins)-2)) {
					b1<-which(tmp>=bins[b]&tmp<bins[b+1])	
					err[[b]]<-(tmp[b1]-y_valid[[j]][,sps][b1,i])/length(b1)
				}
				b1<-which(tmp>=bins[length(bins)-1]&tmp<bins[length(bins)])	
				err[[length(bins)-1]]<-tmp[b1]-y_valid[[j]][,sps][b1,i]
			
				if (length(unlist(err))==0) {
					err<-(tmp<-y_valid[[j]][,sps][,i])/length(tmp)
				}			
				errors<-c(errors,mean(sqrt(unlist(err)^2)))
			}
			probBinRMSE[m,j]<-mean(errors[-1])
		}
	}
		
}

filebody<-paste(RDfinal,dataN[sz],"/probBinRMSE_",sep="")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
if (is.null(ensmblModels)!=TRUE) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
}
save(probBinRMSE, file=paste(filebody,Sets[d],".RData",sep=""))

##########################################################################################

PMs[[4]]<-as.vector(probBinRMSE)
names(PMs)[4]<-"calibration1"

##########################################################################################
