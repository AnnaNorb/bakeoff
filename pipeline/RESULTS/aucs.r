##########################################################################################
# 1b DISCRIMINATION (area under curve)
##########################################################################################
ensmblModels <- opts$modelEnsemble
prevThrs <- opts$prevaleceThreshold

require(pROC)

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
	AUCs <- matrix(NA,nrow=1,ncol=3)
} else {
	AUCs <- matrix(NA,nrow=nmodels,ncol=3)
}

for (j in 1:3) {
	if (is.numeric(prevThrs)) {
		sps<-which((colSums(y_valid[[j]])/nrow(y_valid[[j]]))>=prevThrs)
	} else {
		sps<-1:ncol(y_valid[[j]])
	}
	if (is.null(ensmblModels)!=TRUE) {
		tmp<-matrix(NA,ncol=ncol(y_valid[[j]][,sps]))
			for (i in 1:length(sps)) {
				try(tmp[i]<-auc(predictor=sp_occ_probs[[j]][,i],response=y_valid[[j]][,sps[i]]))
			}
		AUCs[,j] <- mean(tmp,na.rm=T)
	} else {
		for (m in 1:nmodels) {
			tmp<-matrix(NA,ncol=ncol(y_valid[[j]][,sps]))
			for (i in 1:length(sps)) {
				try(tmp[i]<-auc(predictor=sp_occ_probs[[j]][,i,m],response=y_valid[[j]][,sps[i]]))
			}
			length(sp_occ_probs[[j]][,i,m])
			AUCs[m,j] <- mean(tmp,na.rm=T)
		}	
	}	
}

filebody<-paste(RDfinal,dataN[sz],"/AUCs_",sep="")
if (is.numeric(prevThrs)) {
	filebody<-paste(filebody,"spThr",prevThrs*100,"_",sep="")
}
if (is.null(ensmblModels)!=TRUE) {
	if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
	if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
	filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
}
save(AUCs, file=paste(filebody,Sets[d],".RData",sep=""))

PMs[[2]]<-as.vector(AUCs)
names(PMs)[2]<-"discrimination1"

##########################################################################################
# j<-1
# m<-1
# colSums(y_valid[[j]])
# i<-1
# 
# thrshlds<-seq(0,1,by=0.1)
# tpr<-matrix(NA,nrow=length(thrshlds))
# fpr<-matrix(NA,nrow=length(thrshlds))
# tnr<-matrix(NA,nrow=length(thrshlds))
# 
# for (tr in 1:length(thrshlds)) {
# 	TP<-sum((sp_occ_probs[[j]][,i,m]>=thrshlds[tr])[which(y_valid[[j]][,i]==1)])
# 	TN<-sum((sp_occ_probs[[j]][,i,m]<thrshlds[tr])[which(y_valid[[j]][,i]==0)])
# 	FN<-sum((sp_occ_probs[[j]][,i,m]<thrshlds[tr])[which(y_valid[[j]][,i]==1)])
# 	FP<-sum((sp_occ_probs[[j]][,i,m]>=thrshlds[tr])[which(y_valid[[j]][,i]==0)])
# 	tpr[tr,]<-TP/(TP+FN)
# 	fpr[tr,]<-FP/(FP+TN)
# 	tnr[tr,]<-TN/(TN+FP)
# }
# cbind(tpr,fpr)
# 
# quartz()
# par(family="sans",mfrow=c(1,2))
# plot(y=tpr,x=fpr,type="l",ylab="true positive rate",xlab="false positive rate")
# plot(y=tpr,x=tnr,type="l",ylab="true positive rate",xlab="true negative rate",xlim=c(1,0))
# 
# plot(roc(predictor=sp_occ_probs[[j]][,i,m],response=y_valid[[j]][,i]))
# auc(predictor=sp_occ_probs[[j]][,i,m],response=y_valid[[j]][,i])
# 
# hei<-1-fpr[2,] 
# wid1<-1
# wid2<-tpr[2,]
# a1<-hei*((wid1+wid2)/2)
# hei2<-fpr[2,]
# a2<-0.5*wid2*hei2
# print(paste("AUC",round(a1+a2,3)))
# 
