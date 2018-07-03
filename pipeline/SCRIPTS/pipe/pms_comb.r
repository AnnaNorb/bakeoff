rm(list=ls()[!(ls() %in% c("OS","pth","SETT","ENS","PRV"))]); gc(); source(SETT); setwd(WD)

for (e in 1:length(ENS)) {
	for (p in 1:length(PRV)) {
		opts<-list(modelEnsemble=ENS[[e]],prevaleceThreshold=PRV[[p]])		
		for (sz in 1:2) {
			PMS<-NA
			PMs<-list()
			for (d in 1:length(Sets)) {
				source(SETT); set_no <- Sets[d]; source(readdata)
				
				##########################################################################
				# expctME
				filebody<-file.path(RDfinal,dataN[sz],"expctME_")
				if (is.numeric(opts$prevaleceThreshold)) {
					filebody<-paste(filebody,"spThr",opts$prevaleceThreshold*100,"_",sep="")
				}
				if (is.null(opts$modelEnsemble)!=TRUE) {
					if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
					if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
					filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
				}
				load(file=paste(filebody,Sets[d],".RData",sep=""))
				PMs[[1]]<-as.vector(expctME)
				names(PMs)[1]<-"accuracy1"

				# AUCs
				filebody<-file.path(RDfinal,dataN[sz],"AUCs_")
				if (is.numeric(opts$prevaleceThreshold)) {
					filebody<-paste(filebody,"spThr",opts$prevaleceThreshold*100,"_",sep="")
				}
				if (is.null(opts$modelEnsemble)!=TRUE) {
					if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
					if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
					filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
				}
				load(file=paste(filebody,Sets[d],".RData",sep=""))
				PMs[[2]]<-as.vector(AUCs)
				names(PMs)[2]<-"discrimination1"

				# sqrt_pr
				filebody<-file.path(RDfinal,dataN[sz],"sqrt_pr_")
				if (is.numeric(opts$prevaleceThreshold)) {
					filebody<-paste(filebody,"spThr",opts$prevaleceThreshold*100,"_",sep="")
				}
				if (is.null(opts$modelEnsemble)!=TRUE) {
					if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
					if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
					filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
				}
				load(file=paste(filebody,Sets[d],".RData",sep=""))
				PMs[[3]]<-as.vector(sqrt_pr)
				names(PMs)[3]<-"sharpness1"

				# probBinRMSE
				filebody<-file.path(RDfinal,dataN[sz],"probBinRMSE_")
				if (is.numeric(opts$prevaleceThreshold)) {
					filebody<-paste(filebody,"spThr",opts$prevaleceThreshold*100,"_",sep="")
				}
				if (is.null(opts$modelEnsemble)!=TRUE) {
					if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
					if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
					filebody<-paste(filebody,"ensmbl_",ensmbl,"_",sep="")
				}
				load(file=paste(filebody,Sets[d],".RData",sep=""))
				PMs[[4]]<-as.vector(probBinRMSE)
				names(PMs)[4]<-"calibration1"

				# spRichSiteRMSE, BetaRMSE
				filebody1<-file.path(RDfinal,dataN[sz],"spRichSiteRMSE_")
				filebody2<-file.path(RDfinal,dataN[sz],"BetaRMSE_")
				if (is.numeric(opts$prevaleceThreshold)) {
					filebody1<-paste(filebody1,"spThr",opts$prevaleceThreshold*100,"_",sep="")
					filebody2<-paste(filebody2,"spThr",opts$prevaleceThreshold*100,"_",sep="")
				}
				if (is.null(opts$modelEnsemble)!=TRUE) {
					if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
					if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
					filebody1<-paste(filebody1,"ensmbl_",ensmbl,"_",sep="")
					filebody2<-paste(filebody2,"ensmbl_",ensmbl,"_",sep="")
				}
				load(file=paste(filebody1,Sets[d],".RData",sep=""))
				load(file=paste(filebody2,Sets[d],".RData",sep=""))
				PMs[[5]]<-as.vector(spRichSiteRMSE)
				names(PMs)[5]<-"accuracy2site"
				for (b in 1:3) {
					PMs[[(5+b)]]<-as.vector(BetaRMSE[[b]])
					names(PMs)[(5+b)]<-paste("accuracy3beta",b,sep="")
				}

				# spRichSiteSpear, betaIndSpear
				filebody1<-file.path(RDfinal,dataN[sz],"/spRichSiteSpear_")
				filebody2<-file.path(RDfinal,dataN[sz],"/betaIndSpear_")
				if (is.numeric(opts$prevaleceThreshold)) {
					filebody1<-paste(filebody1,"spThr",opts$prevaleceThreshold*100,"_",sep="")
					filebody2<-paste(filebody2,"spThr",opts$prevaleceThreshold*100,"_",sep="")
				}
				if (is.null(opts$modelEnsemble)!=TRUE) {
					if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
					if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
					filebody1<-paste(filebody1,"ensmbl_",ensmbl,"_",sep="")
					filebody2<-paste(filebody2,"ensmbl_",ensmbl,"_",sep="")
				}
				load(file=paste(filebody1,Sets[d],".RData",sep=""))
				load(file=paste(filebody2,Sets[d],".RData",sep=""))
				if (is.null(opts$modelEnsemble)!=TRUE) {
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

				# spRichSiteSD, betaIndSD
				filebody1<-file.path(RDfinal,dataN[sz],"/spRichSiteSD_")
				filebody2<-file.path(RDfinal,dataN[sz],"/betaIndSD_")
				if (is.numeric(opts$prevaleceThreshold)) {
					filebody1<-paste(filebody1,"spThr",opts$prevaleceThreshold*100,"_",sep="")
					filebody2<-paste(filebody2,"spThr",opts$prevaleceThreshold*100,"_",sep="")
				}
				if (is.null(opts$modelEnsemble)!=TRUE) {
					if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
					if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
					filebody1<-paste(filebody1,"ensmbl_",ensmbl,"_",sep="")
					filebody2<-paste(filebody2,"ensmbl_",ensmbl,"_",sep="")
				}
				load(file=paste(filebody1,Sets[d],".RData",sep=""))
				load(file=paste(filebody2,Sets[d],".RData",sep=""))
				if (is.null(opts$modelEnsemble)!=TRUE) {
					tmp1<-colMeans(spRichSiteSD)
					tmp3<-lapply(betaIndSD,colMeans)
				} else {
					tmp1<-apply(spRichSiteSD,3,rowMeans)
					tmp3<-list()
					for (b in 1:3) {
						tmp3[[b]]<-apply(betaIndSD[[b]],3,rowMeans)
					}
				}
				PMs[[13]]<-as.vector(tmp1)
				names(PMs)[13]<-"sharpness2site"
				for (b in 1:3) {
					PMs[[(13+b)]]<-as.vector(tmp3[[b]])
					names(PMs)[(13+b)]<-paste("sharpness3beta",b,sep="")
				}


				# f50_sprichSite, f50_Betas
				filebody1<-file.path(RDfinal,dataN[sz],"/f50_sprichSite_")
				filebody2<-file.path(RDfinal,dataN[sz],"/f50_Betas_")
				if (is.numeric(opts$prevaleceThreshold)) {
					filebody1<-paste(filebody1,"spThr",opts$prevaleceThreshold*100,"_",sep="")
					filebody2<-paste(filebody2,"spThr",opts$prevaleceThreshold*100,"_",sep="")
				}
				if (is.null(opts$modelEnsemble)!=TRUE) {
					if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
					if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
					filebody1<-paste(filebody1,"ensmbl_",ensmbl,"_",sep="")
					filebody2<-paste(filebody2,"ensmbl_",ensmbl,"_",sep="")
				}
				load(file=paste(filebody1,Sets[d],".RData",sep=""))
				load(file=paste(filebody2,Sets[d],".RData",sep=""))
				tmp1<-abs(0.5-f50_sprichSite)
				tmp3<-list()
				for (b in 1:3) {
					tmp3[[b]]<-abs(0.5-f50_Betas[[b]])
				}
				PMs[[17]]<-as.vector(tmp1)
				names(PMs)[17]<-"calibration2site"
				for (b in 1:3) {
					PMs[[(17+b)]]<-as.vector(tmp3[[b]])
					names(PMs)[(17+b)]<-paste("calibration3beta",b,sep="")
				}

				##########################################################################

				pms<-simplify2array(PMs)
				PMS<-rbind(PMS,pms)
			}
			PMS<-PMS[-1,]

			filebody<-file.path(RDfinal,dataN[sz],"meta_analysis","PMS")
			if (is.numeric(opts$prevaleceThreshold)) {
				filebody<-paste(filebody,"_spThr",opts$prevaleceThreshold*100,sep="")
			}
			if (is.null(opts$modelEnsemble)!=TRUE) {
				if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
				if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
				filebody<-paste(filebody,"_ensmbl_",ensmbl,sep="")
			}
			save(PMS, file=paste(filebody,".RData",sep=""))
		}
	}
}

