##########################################################################################
# BAKEOFF PIPELINE
##########################################################################################

rm(list = ls(all=TRUE)) 
gc()

##########################################################################################

OS<-"osx"
#OS<-"win"

##########################################################################################

SETT<-".../bakeoff/pipeline/SCRIPTS/settings.r"
source(SETT)
setwd(WD)

##########################################################################################

# dir.create(PD2)
# dir.create(RD)
# for (set in Sets) {
# 	dir.create(paste(RD,set,sep=""))
# 	}
# dir.create(RDfinal)
# dir.create(paste(RDfinal,"300/",sep=""))
# dir.create(paste(RDfinal,"600/",sep=""))
# dir.create(paste(RDfinal,"custom/",sep=""))

##########################################################################################

install.packages(c("pROC","mvabund","MultivariateRandomForest","randomForest","caret",
					"e1071","gbm","dismo","yaImpute","earth","devtools","glmnet",
					"boral","gjam","spaMM","nlme","MASS","spaMM","vegan","BayesComm"))
require(devtools)
install_github("davharris/mistnet2")

if (OS=="osx") { 
	install.packages("doMC")
	install.packages(paste(WD,"MODELS/mvpart_pkg/mvpart_1.6-2.tar",sep=''), repos = NULL, type="source") 
	}
if (OS=="win") {
	install.packages("doParallel")
	install.packages(paste(WD,"MODELS/mvpart_pkg/mvpart_1.6-2.zip",sep=''), repos = NULL, type="source")
	}

##########################################################################################

crs<-2
if (OS=="osx") {
	require(doMC)
	registerDoMC(cores=crs)
}
if (OS=="win") {
	require(doParallel)
	registerDoParallel(cl=makeCluster(crs))
}

##########################################################################################

sz<-1
#sz<-2
#sz<-3
	#customData<-list(1:100,1:10)	# [[1]] #sampling units [[2]] #species

##########################################################################################

for (d in 1:length(Sets)) {

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

	# ss GLM 1
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.glm.r",sep=""))

	# ss GLM 2
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.glmmPQL.r",sep=""))

	# ss GLM 3
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.spaMM.r",sep=""))
	
	# TRAITGLM
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.traitglm.r",sep=""))
	  
	# GAM
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.gam.r",sep=""))

	# BRT
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.brt.r",sep=""))
  
	# SVM
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.svm.r",sep=""))
	
	# RFs
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.rf.r",sep=""))
	
	# GNNs
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.gnn.r",sep=""))
	
	# MRTs
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.mrt.r",sep=""))
	  
	# MARS
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.mars.r",sep=""))
		
	# GJAMS
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.gjam.r",sep=""))
	
	# BORAL
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.boral.r",sep=""))

	# SAMs
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.sam.r",sep=""))

	# mistnet
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.mistnet.r",sep=""))
	  
	# SPBAYES
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.spbayes.r",sep=""))

	# BayesComm
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.bc.r",sep=""))

	# MVRF
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.mvrf.r",sep=""))

	# ssHMSC
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.hmsc.ss.r",sep=""))

	# HMSC
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(readdata); setwd(WD)
	source(paste(PD,"predict.hmsc.all.r",sep=""))	
	
	}


##########################################################################################
# PERFORMANCE MEASURES
##########################################################################################

rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); setwd(WD)

sz<-1
#sz<-2
#sz<-3
#customData<-list(100,10)	# [[1]] #sampling units [[2]] #species

for (d in 1:length(Sets)) {

	set_no <- Sets[d]
	source(readdata)
	source(modpreds)		

	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata); setwd(WD)
	source(paste(RD,"aucs.r",sep=""))

	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata); setwd(WD)
	source(paste(RD,"liks.r",sep=""))

	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata); setwd(WD)
	source(paste(RD,"mse.r",sep=""))

	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata); setwd(WD)
	source(paste(RD,"spearm.r",sep=""))

	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata); setwd(WD)
	source(paste(RD,"sds.r",sep=""))

	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata); setwd(WD)
	source(paste(RD,"p50.r",sep=""))

	}	
	
##########################################################################################
