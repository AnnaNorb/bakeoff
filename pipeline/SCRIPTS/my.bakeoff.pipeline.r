##########################################################################################
# BAKEOFF PIPELINE
##########################################################################################

rm(list = ls(all=TRUE)) 
gc()

# remove this part for the github version
##########################################################################################

info<-Sys.info()
if (substr(info[4],1,3)=="LM7" | substr(info[4],1,5)=="staff"){
	SETT<-"~/OneDrive - University of Helsinki/bakeoff/pipeline/SCRIPTS/mySettings.r"
} else {
	SETT<-"D:/HY-data/NORBERG/OneDrive - University of Helsinki/bakeoff/pipeline/SCRIPTS/mySettings.r"
}

##########################################################################################

#SETT<-".../bakeoff/pipeline/settings.r"
source(SETT)
setwd(WD)

# install required packages
	# install.packages(c("pROC","mvabund","MultivariateRandomForest","randomForest","caret",
						# "e1071","gbm","dismo","yaImpute","earth","devtools","glmnet",
						# "boral","gjam","spaMM","nlme","MASS","spaMM","vegan","BayesComm"))
	# require(devtools)
	# install_github("davharris/mistnet2")

	# mac
		# install.packages("doMC")
		# install.packages(paste(WD,"MODELS/mvpart_pkg/mvpart_1.6-2.tar",sep=''), repos = NULL, type="source")
	# windows
		# install.packages("doParallel")
		# install.packages(paste(WD,"MODELS/mvpart_pkg/mvpart_1.6-2.zip",sep=''), repos = NULL, type="source")

##########################################################################################

# data size
sz<-1	# small
#sz<-2	# large
#sz<-3	# custom
	#customData<-list(1:100,1:10)	# [[1]] #sampling units [[2]] #species

##########################################################################################

#for (d in 1:length(Sets)) {	# loop over data sets

d<-2
set_no <- Sets[d]
source(readdata)


	# FIT MODELS
	##############
		
	source(paste(MD,"fit.glm.r",sep=""))		# ssGLM 1
	source(paste(MD,"fit.glmmPQL.r",sep=""))	# ssGLM 2
	source(paste(MD,"fit.gam.r",sep=""))		# GAM
	source(paste(MD,"fit.spaMM.r",sep=""))		# ssGLM 3
	source(paste(MD,"fit.mrt.r",sep=""))		# MRTs
	source(paste(MD,"fit.rf.r",sep=""))			# RFs
	source(paste(MD,"fit.brt.r",sep=""))		# BRT
	source(paste(MD,"fit.svm.r",sep=""))		# SVM
	source(paste(MD,"fit.gnn.r",sep=""))		# GNNs
	source(paste(MD,"fit.mars.r",sep=""))		# MARS
	source(paste(MD,"fit.gjam.r",sep=""))  		# GJAMS
	source(paste(MD,"fit.mistnet.r",sep=""))  	# mistnet
	source(paste(MD,"fit.sam.r",sep=""))		# SAMs
	source(paste(MD,"fit.bc.r",sep=""))			# BayesComm
	source(paste(MD,"fit.boral.r",sep=""))  	# BORAL 
	source(paste(MD,"fit.traitglm.r",sep=""))	# MVABUND
	source(paste(MD,"fit.mvrf.r",sep=""))  		# MVRF
	source(paste(MD,"fit.spbayes.r",sep=""))	# SPBAYES


	# GET PREDICTIONS
	###################

	# ssHMSC
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.hmsc.ss.r",sep=""))
  	#write.table(y_train[[i]],file=paste("Yy_",set_no,"_",i,"_sz_",sz,"_training.csv",sep=""),col.names=FALSE,row.names=FALSE,sep=",")
	#write.table(y_valid[[i]],file=paste("Yy_",set_no,"_",i,"_sz_",sz,"_validation.csv",sep=""),col.names=FALSE,row.names=FALSE,sep=",")

	# ss GLM 1
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.glm.r",sep=""))

	# ss GLM 2
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.glmmPQL.r",sep=""))

	# ss GLM 3
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.spaMM.r",sep=""))
	
	# TRAITGLM
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.traitglm.r",sep=""))
	  
	# GAM
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.gam.r",sep=""))

	# BRT
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.brt.r",sep=""))
  
	# SVM
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.svm.r",sep=""))
	
	# RFs
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.rf.r",sep=""))
	
	# GNNs
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.gnn.r",sep=""))
	
	# MRTs
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.mrt.r",sep=""))
	  
	# MARS
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.mars.r",sep=""))
		
	# GJAMS
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.gjam.r",sep=""))
	
	# BORAL
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.boral.r",sep=""))

	# SAMs
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.sam.r",sep=""))

	# mistnet
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.mistnet.r",sep=""))
	  
	# SPBAYES
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.spbayes.r",sep=""))

	# BayesComm
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.bc.r",sep=""))

	# MVRF
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.mvrf.r",sep=""))

	# HMSC
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.hmsc.all.r",sep=""))
		#write.table(y_train[[i]],file=paste("Yy_",set_no,"_",i,"_sz_",sz,"_training.csv",sep=""),col.names=FALSE,row.names=FALSE,sep=",")
		#write.table(y_valid[[i]],file=paste("Yy_",set_no,"_",i,"_sz_",sz,"_validation.csv",sep=""),col.names=FALSE,row.names=FALSE,sep=",")
	




##########################################################################################
# PERFORMANCE MEASURES
##########################################################################################

rm(list = ls(all=TRUE)) 
gc()

info<-Sys.info()
if (substr(info[4],1,3)=="LM7" | substr(info[4],1,5)=="staff"){
	SETT<-"~/OneDrive - University of Helsinki/bakeoff/pipeline/SCRIPTS/mySettings.r"
} else {
	SETT<-"D:/HY-data/NORBERG/OneDrive - University of Helsinki/bakeoff/pipeline/SCRIPTS/mySettings.r"
}

source(SETT)
setwd(WD)

# data size
#sz<-1	# small
#sz<-2	# large
#sz<-3	# custom
#customData<-list(100,10)	# [[1]] #sampling units [[2]] #species

#for (sz in 1:2) {	# loop over data sizes

#	for (d in 1:length(Sets)) {	# loop over data sets

sz<-1
d<-1

		set_no <- Sets[d]
		source(readdata)

		#source(modpreds)		

		rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
		source(paste(RD,"aucs.r",sep=""))

		rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
		source(paste(RD,"liks.r",sep=""))

		rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
		source(paste(RD,"mse.r",sep=""))

		rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
		source(paste(RD,"spearm.r",sep=""))

		rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
		source(paste(RD,"sds.r",sep=""))

		rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
		source(paste(RD,"p50.r",sep=""))

		}
	}	
	
##########################################################################################
# FIGURES
##########################################################################################

rm(list = ls(all=TRUE)) 
gc()

info<-Sys.info()
if (substr(info[4],1,3)=="LM7" | substr(info[4],1,5)=="staff"){
	SETT<-"~/OneDrive - University of Helsinki/bakeoff/pipeline/SCRIPTS/mySettings.r"
} else {
	SETT<-"D:/HY-data/NORBERG/OneDrive - University of Helsinki/bakeoff/pipeline/SCRIPTS/mySettings.r"
}

source(SETT)
setwd(WD)

# data size
#sz<-1	# small
#sz<-2	# large
#sz<-3	# custom
#customData<-list(100,10)	# [[1]] #sampling units [[2]] #species

titlz<-c("Interpolation","Extrapolation 1","Extrapolation 2")
betaTitlz<-c("turnover (Sim)","nestedness","overall diversity (Sör)")
#for (sz in 1:2) {	# loop over data sizes

#	for (d in 1:length(Sets)) {	# loop over data sets

sz<-1
d<-1

set_no <- Sets[d]
source(readdata)	

### NEW

load(file=paste(RDfinal,dataN[sz],"/LIKs_",Sets[d],".RData",sep=""))
LIKs<-LIKs*-1
pdf(file=paste(RDfinal,dataN[sz],"/figs/loglik.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(LIKs[,j],main=titlz[j],xlab="",ylab="-1*log-likelihood",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,LIKs[i,j]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Accuracy and sharpness of occurrence probs",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/AUCs_",Sets[d],".RData",sep=""))
pdf(file=paste(RDfinal,dataN[sz],"/figs/auc.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(AUCs[,j],main=titlz[j],xlab="",ylab="AUC",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,AUCs[i,j]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Discrimination in occurrence probs",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/spRichSiteMSE_",Sets[d],".RData",sep=""))
pdf(file=paste(RDfinal,dataN[sz],"/figs/spRichSiteMSE.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(spRichSiteMSE[,j],main=titlz[j],xlab="",ylab="MSE(sp richness per site)",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,spRichSiteMSE[i,j]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Accuracy in species richness at site level",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/spRichAreaMSE_",Sets[d],".RData",sep=""))
pdf(file=paste(RDfinal,dataN[sz],"/figs/spRichAreaMSE.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(spRichAreaMSE[,j],main=titlz[j],xlab="",ylab="MSE(sp richness per area)",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,spRichAreaMSE[i,j]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Accuracy in species richness at area level",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/BetaMSE_",Sets[d],".RData",sep=""))
for (b in 1:3) {
	pdf(file=paste(RDfinal,dataN[sz],"/figs/BetaMSE",b,".pdf",sep=""),bg="transparent",width=10,height=8)
	par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
	for (j in 1:3) {
		plot(BetaMSE[[b]][,j],main=titlz[j],xlab="",ylab="MSE",xaxt="n",pch=16)
		for (i in 1:length(mod_names)) {
			lines(y=c(0,BetaMSE[[b]][i,j]),x=c(i,i))
			}
			axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
		}
	mtext(side=3,text=paste("Accuracy in beta index: ",betaTitlz[b],sep=""),outer=T,line=1)
	dev.off()
}

load(file=paste(RDfinal,dataN[sz],"/spRichSiteSpear_",Sets[d],".RData",sep=""))
pdf(file=paste(RDfinal,dataN[sz],"/figs/spRichSiteSpear.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(rowMeans(spRichSiteSpear[,,j],na.rm=T),main=titlz[j],xlab="",ylab="Spearman's correlation",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,rowMeans(spRichSiteSpear[,,j],na.rm=T)[i]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Discrimination in species richness at sites",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/spRichAreaSpear_",Sets[d],".RData",sep=""))
pdf(file=paste(RDfinal,dataN[sz],"/figs/spRichAreaSpear.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(rowMeans(spRichAreaSpear[,,j],na.rm=T),main=titlz[j],xlab="",ylab="Spearman's correlation",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,rowMeans(spRichAreaSpear[,,j],na.rm=T)[i]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Discrimination in species richness at areas",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/betaIndSpear_",Sets[d],".RData",sep=""))
for (b in 1:3) {
	pdf(file=paste(RDfinal,dataN[sz],"/figs/betaIndSpear",b,".pdf",sep=""),bg="transparent",width=10,height=8)
	par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
	for (j in 1:3) {
		plot(rowMeans(betaIndSpear[[b]][,,j],na.rm=T),main=titlz[j],xlab="",ylab="Spearman's correlation",xaxt="n",pch=16)
		for (i in 1:length(mod_names)) {
			lines(y=c(0,rowMeans(betaIndSpear[[b]][,,j],na.rm=T)[i]),x=c(i,i))
			}
			axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
		}
	mtext(side=3,text=paste("Discrimination in beta index: ",betaTitlz[b],sep=""),outer=T,line=1)
	dev.off()
}

load(file=paste(RDfinal,dataN[sz],"/spRichSiteSD_",Sets[d],".RData",sep=""))
pdf(file=paste(RDfinal,dataN[sz],"/figs/spRichSiteSD.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(rowMeans(spRichSiteSD[,,j],na.rm=T),main=titlz[j],xlab="",ylab="SD",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,rowMeans(spRichSiteSD[,,j],na.rm=T)[i]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Sharpness of sp richness at site level",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/spRichAreaSD_",Sets[d],".RData",sep=""))
pdf(file=paste(RDfinal,dataN[sz],"/figs/spRichAreaSD.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(rowMeans(spRichAreaSD[,,j],na.rm=T),main=titlz[j],xlab="",ylab="SD",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,rowMeans(spRichAreaSD[,,j],na.rm=T)[i]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Sharpness of sp richness at area level",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/betaIndSD_",Sets[d],".RData",sep=""))
for (b in 1:3) {
	pdf(file=paste(RDfinal,dataN[sz],"/figs/betaIndSD",b,".pdf",sep=""),bg="transparent",width=10,height=8)
	par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
	for (j in 1:3) {
		plot(rowMeans(betaIndSD[[b]][,,j],na.rm=T),main=titlz[j],xlab="",ylab="SD",xaxt="n",pch=16)
		for (i in 1:length(mod_names)) {
			lines(y=c(0,rowMeans(betaIndSD[[b]][,,j],na.rm=T)[i]),x=c(i,i))
			}
			axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
		}
	mtext(side=3,text=paste("Sharpness of beta index: ",betaTitlz[b],sep=""),outer=T,line=1)
	dev.off()
	}

load(file=paste(RDfinal,dataN[sz],"/f50_sprichSite_",Sets[d],".RData",sep=""))
f50_sprichSite<-abs(f50_sprichSite-0.5)
pdf(file=paste(RDfinal,dataN[sz],"/figs/f50_sprichSite.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(f50_sprichSite[,j],main=titlz[j],xlab="",ylab="Bias in 50% CI",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,f50_sprichSite[i,j]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Validity of prediction interval in sp richness per site",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/f50_sprichArea_",Sets[d],".RData",sep=""))
f50_sprichArea<-abs(f50_sprichArea-0.5)
pdf(file=paste(RDfinal,dataN[sz],"/figs/f50_sprichArea.pdf",sep=""),bg="transparent",width=10,height=8)
par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
for (j in 1:3) {
	plot(f50_sprichArea[,j],main=titlz[j],xlab="",ylab="Bias in 50% CI",xaxt="n",pch=16)
	for (i in 1:length(mod_names)) {
		lines(y=c(0,f50_sprichArea[i,j]),x=c(i,i))
		}
		axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
	}
mtext(side=3,text="Validity of prediction interval in sp richness per area",outer=T,line=1)
dev.off()

load(file=paste(RDfinal,dataN[sz],"/f50_Betas_",Sets[d],".RData",sep=""))
for (b in 1:3) {
	pdf(file=paste(RDfinal,dataN[sz],"/figs/f50_Betas",b,".pdf",sep=""),bg="transparent",width=10,height=8)
	par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
	for (j in 1:3) {
		plot(f50_Betas[[b]][,j],main=titlz[j],xlab="",ylab="SD",xaxt="n",pch=16)
		for (i in 1:length(mod_names)) {
			lines(y=c(0,f50_Betas[[b]][i,j]),x=c(i,i))
			}
			axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
		}
	mtext(side=3,text=paste("Validity of prediction interval in beta index: ",betaTitlz[b],sep=""),outer=T,line=1)
	dev.off()
	}


### OLD

		load(file=paste(RDfinal,dataN[sz],"/TJURS_",Sets[d],".RData",sep=""))
			pdf(file=paste(RDfinal,dataN[sz],"/figs/tjurs.pdf",sep=""),bg="transparent",width=10,height=8)
			par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
			for (j in 1:3) {
				plot(rowMeans(TJURS[,,j],na.rm=T),main=titlz[j],xlab="",ylab="Tjur R",xaxt="n",pch=16)
					for (i in 1:length(mod_names)) {
						lines(y=c(0,rowMeans(TJURS[,,j],na.rm=T)[i]),x=c(i,i))
					}
				axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
			}
			mtext(side=3,text="Tjur R",outer=T,line=1)
			dev.off()

					
		load(file=paste(RDfinal,dataN[sz],"/DEVS_",Sets[d],".RData",sep=""))
			pdf(file=paste(RDfinal,dataN[sz],"/figs/devs.pdf",sep=""),bg="transparent",width=10,height=8)
			par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
			for (j in 1:3) {
				plot(DEVS[,j],main=titlz[j],xlab="",ylab="Deviances",xaxt="n",pch=16)
					for (i in 1:length(mod_names)) {
						lines(y=c(0,DEVS[i,j]),x=c(i,i))
					}
				axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
			}
			mtext(side=3,text="Deviances",outer=T,line=1)
			dev.off()
	
		load(file=paste(RDfinal,dataN[sz],"/spRichSpearm_",Sets[d],".RData",sep=""))
			pdf(file=paste(RDfinal,dataN[sz],"/figs/spRicSpearm.pdf",sep=""),bg="transparent",width=10,height=8)
			par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
			for (j in 1:3) {
				plot(rowMeans(spRichSpearm[,,j],na.rm=T),main=titlz[j],xlab="",ylab="sp richness spearman",xaxt="n",pch=16)
					for (i in 1:length(mod_names)) {
						lines(y=c(0,rowMeans(spRichSpearm[,,j],na.rm=T)[i]),x=c(i,i))
					}
				axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
			}
			mtext(side=3,text="sp richness spearman",outer=T,line=1)
			dev.off()
		
		load(file=paste(RDfinal,dataN[sz],"/spRichCI95err_",Sets[d],".RData",sep=""))
			pdf(file=paste(RDfinal,dataN[sz],"/figs/spRicCIerr.pdf",sep=""),bg="transparent",width=10,height=8)
			par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
			for (j in 1:3) {
				plot(spRichCI95err[,j],main=titlz[j],xlab="",ylab="Error in sp richness CI",xaxt="n",pch=16)
					for (i in 1:length(mod_names)) {
						lines(y=c(0,spRichCI95err[i,j]),x=c(i,i))
					}
				axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
			}
			mtext(side=3,text="Error in sp richness CI",outer=T,line=1)
			dev.off()
		
		load(file=paste(RDfinal,dataN[sz],"/spRichAreaErr2means_",Sets[d],".RData",sep=""))
			pdf(file=paste(RDfinal,dataN[sz],"/figs/spRicAreaErr.pdf",sep=""),bg="transparent",width=10,height=8)
			par(mar=c(8,4,2,2),mfrow=c(3,1),oma=c(0,0,3,0))		
			for (j in 1:3) {
				plot(spRichAreaErr2means[,j],main=titlz[j],xlab="",ylab="MSE",xaxt="n",pch=16)
					for (i in 1:length(mod_names)) {
						lines(y=c(0,spRichAreaErr2means[i,j]),x=c(i,i))
					}
				axis(side=1,labels=unlist(mod_names),at=1:length(mod_names),las=2)	
			}
			mtext(side=3,text="MSE in sp richness per area",outer=T,line=1)
			dev.off()	
	
	
		}
	}	
	
	