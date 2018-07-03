##########################################################################################
# PERFORMANCE MEASURES	
##########################################################################################
rm(list=ls()[!(ls() %in% c("OS","pth","SETT","ENS","PRV"))]); gc(); source(SETT); setwd(WD)

for (e in 1:length(ENS)) {
	for (p in 1:length(PRV)) {
		opts<-list(modelEnsemble=ENS[[e]],prevaleceThreshold=PRV[[p]])		
		for (sz in 1:2) {
			PMS<-NA
			PMs<-list()
			for (d in 1:length(Sets)) {
				source(SETT); set_no <- Sets[d]; source(readdata)
				source(file.path(RD,"expct.r"))
				source(file.path(RD,"aucs.r"))
				source(file.path(RD,"sqrtPrs.r"))
				source(file.path(RD,"probBinRMSE.r"))
				source(file.path(RD,"mse.r"))
				source(file.path(RD,"spearm.r"))
				source(file.path(RD,"sds.r"))
				source(file.path(RD,"p50.r"))
# 				pms<-simplify2array(PMs)
# 				PMS<-rbind(PMS,pms)
			}
# 			PMS<-PMS[-1,]

# 			filebody<-file.path(RDfinal,dataN[sz],"meta_analysis","PMS")
# 			if (is.numeric(prevThrs)) {
# 				filebody<-paste(filebody,"_spThr",prevThrs*100,sep="")
# 			}
# 			if (is.null(ensmblModels)!=TRUE) {
# 				if (length(ensmblModels)==nmodels) { ensmbl<-"all" }
# 				if (length(ensmblModels)!=nmodels) { ensmbl<-paste(ensmblModels,collapse="_") }
# 				filebody<-paste(filebody,"_ensmbl_",ensmbl,sep="")
# 			}
# 			save(PMS, file=paste(filebody,".RData",sep=""))
		}
	}
}
##########################################################################################
