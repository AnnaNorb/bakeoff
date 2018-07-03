##########################################################################################
# GET PREDICTIONS
##########################################################################################

# ss GLM 1
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.glm.r",sep=""))

# ss GLM 2
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.glmmPQL.r",sep=""))

# BRT
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.brt.r",sep=""))

# SVM
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.svm.r",sep=""))

# GNNs
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.gnn.r",sep=""))

# MARS
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.mars.r",sep=""))

# MVABUND
	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.manyglm.r",sep=""))

	rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
	source(paste(PD,"predict.traitglm.r",sep=""))

# GAM
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.gam.r",sep=""))

# RFs
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.rf.r",sep=""))

# MRTs
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.mrt.r",sep=""))

# GJAMS
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.gjam.r",sep=""))

# SAMs
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.sam.r",sep=""))

# mistnet
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.mistnet.r",sep=""))

# BayesComm
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.bc.r",sep=""))

# BORAL
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.boral.r",sep=""))

# ssHMSC
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.hmsc.ss.r",sep=""))

# HMSC
rm(list=ls()[!(ls() %in% saveobjs)]); gc(); source(SETT); source(readdata)
source(paste(PD,"predict.hmsc.all.r",sep=""))

##########################################################################################
