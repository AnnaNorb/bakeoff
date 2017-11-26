##########################################################################################
# PIPELINE FOR REAL DATA
##########################################################################################

rm(list = ls(all=TRUE)) 
gc()

SETT<-'.../pipeline/settings.r'
SETT<-'~/OneDrive - University of Helsinki/bakeoff/pipeline/settings.r'
source(SETT)
setwd(WD)

# matrix for computation times
comTimes<-rep( list(list()), nmodels ) 
names(comTimes)<-mod_names

# pipeline
for (d in 1:length(Sets)) {

set_no <- Sets[d]

# READ THE CHOSEN DATA
source(readdata)

# FIT MODELS
##########################################################################################
  
# ssGLM 1
source(paste(MD,"glm/fit.glm.r",sep=""))

# ssGLM 2
source(paste(MD,"glm/fit.glmmPQL.r",sep=""))

# ssGLM 3
source(paste(MD,"glm/fit.spaMM.r",sep=""))

# MANYGLM
source(paste(MD,"mvabund/fit.manyglm.r",sep=""))

# GAM
source(paste(MD,"gam/fit.gam.new.r",sep=""))

# MRTs
source(paste(MD,"mrt/fit.mrt.r",sep=""))
  
# RFs
source(paste(MD,"rf/fit.rf.r",sep=""))

# BRT
source(paste(MD,"brt/fit.brt.r",sep=""))
  
# SVM
source(paste(MD,"svm/fit.svm.r",sep=""))

# GNNs
source(paste(MD,"gnn/fit.gnn.r",sep=""))
  
# MARS
source(paste(MD,"mars/fit.mars.r",sep=""))
 
# GJAMS
source(paste(MD,"gjam/fit.gjam.r",sep=""))  
 
# mistnet
source(paste(MD,"mistnet/fit.mistnet.r",sep=""))  

# MVRF
source(paste(MD,"mvrf/fit.mvrf.r",sep=""))  

# SAMs
source(paste(MD,"sam/fit.sam.r",sep=""))

##########################################################################################
save(comTimes, file=paste(WD,"comTimes_",set_no,'.r',sep=""))
 

# GET PREDICTIONS
##########################################################################################
 
# ssHMSC
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.hmsc.ss.r",sep=""))
  
# ss GLM 1
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.glm.r",sep=""))

# ss GLM 2
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.glmmPQL.r",sep=""))

# ss GLM 3
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.spaMM.r",sep=""))

# MANYGLM
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.manyglm.r",sep=""))
  
# GAM
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.gam.r",sep=""))

# BRT
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.brt.r",sep=""))

# SVM
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.svm.r",sep=""))

# RF
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.rf.r",sep=""))

# GNN
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.gnn.r",sep=""))

# MRT
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.mrt.r",sep=""))
  
# MARS
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.mars.r",sep=""))

# MVRF
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.mvrf.r",sep=""))

# GJAMS
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.gjam.r",sep=""))

# SAM
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.sam.r",sep=""))
    
# mistnet
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.mistnet.r",sep=""))
  
# HMSC
rm(list=ls()[!(ls() %in% saveobjs)])
gc()
source(SETT)
source(readdata)
source(paste(PD,"predict.hmsc.all.r",sep=""))

}

##########################################################################################
