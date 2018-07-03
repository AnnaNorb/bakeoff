##########################################################################################
# full PM results table with features without any tranformations
##########################################################################################
rm(list=ls()[!(ls() %in% c("OS","pth","SETT","ENS","PRV"))]); gc(); source(SETT); setwd(WD)

TBL_PMS_ALL<-NA

for (e in 1:length(ENS)) {
	for (p in 1:length(PRV)) {
		opts<-list(modelEnsemble=ENS[[e]],prevaleceThreshold=PRV[[p]])		
		#TBL_PMS_all<-NA
		for (sz in 1:2) {
			filebody<-file.path(RDfinal,dataN[sz],"/meta_analysis/PMS")
			if (is.numeric(opts$prevaleceThreshold)) {
				filebody<-paste(filebody,"_spThr",opts$prevaleceThreshold*100,sep="")
			}
			if (is.null(opts$modelEnsemble)!=TRUE) {
				if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
				if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
				filebody<-paste(filebody,"_ensmbl_",ensmbl,sep="")
			}
			load(file=paste(filebody,".RData",sep=""))
	
			noMod<-dim(PMS)[1]/(3*length(Sets))

			if (is.null(opts$modelEnsemble)!=TRUE) {
				tblrep<-matrix(rep(c(paste("ENS",ensmbl,sep="_"),"ensemble",1*(colSums(feats[ENS[[e]],3:ncol(feats)])>0)),each=(length(Sets)*3)),nrow=length(Sets)*3)
				colnames(tblrep)<-colnames(feats)
				TBL<-cbind(tblrep,dataN[sz],rep(rep(c("i","e1","e2"),each=noMod),length(Sets)),rep(Sets,each=(noMod*3)))
			} else {
				tblrep<-apply(feats,2,rep,times=3*length(Sets))
				TBL<-cbind(tblrep,dataN[sz],rep(rep(c("i","e1","e2"),each=noMod),length(Sets)),rep(Sets,each=(noMod*3)))
			}
			colnames(TBL)[(ncol(TBL)-2):ncol(TBL)]<-c("dataSize","predType","dataSet")
			TBL_PMS<-cbind(TBL,PMS,PRV[[p]])

			colnames(TBL_PMS)[ncol(TBL_PMS)]<-"prevThr"
			#print(paste("Number of NAs",sum(is.na(TBL_PMS)),"for data size",dataN[sz]))
			#TBL_PMS_all<-rbind(TBL_PMS_all,TBL_PMS)			
			#TBL_PMS_all<-TBL_PMS_all[-1,]

			filebody<-file.path(RDfinal,dataN[sz],"meta_analysis","finalTBLall")
			if (is.numeric(opts$prevaleceThreshold)) {
				filebody<-paste(filebody,"_spThr",opts$prevaleceThreshold*100,sep="")
			}
			if (is.null(opts$modelEnsemble)!=TRUE) {
				if (length(opts$modelEnsemble)==nmodels) { ensmbl<-"all" }
				if (length(opts$modelEnsemble)!=nmodels) { ensmbl<-paste(opts$modelEnsemble,collapse="_") }
				filebody<-paste(filebody,"_ensmbl_",ensmbl,sep="")
			}
			save(TBL_PMS, file=paste(filebody,".RData",sep=""))
			TBL_PMS_ALL<-rbind(TBL_PMS_ALL,TBL_PMS)
		}
	}
}
TBL_PMS_ALL<-TBL_PMS_ALL[-1,]
rownames(TBL_PMS_ALL)<-NULL
if ( dim(TBL_PMS_ALL)[1]!=(length(PRV)*2*length(Sets)*(length(mod_names)+2)*3) ){
	stop("Wrong amount of rows in 'TBL_PMS_ALL'")
}
# head(TBL_PMS_ALL)
# tail(TBL_PMS_ALL)
# dim(TBL_PMS_ALL)
save(TBL_PMS_ALL, file=file.path(RDfinal,"TBL_PMS_ALL.RData"))
write.table(TBL_PMS_ALL,file=file.path(RDfinal,"TBL_PMS_ALL.csv"),sep=",",row.names=F,col.names=T)					
