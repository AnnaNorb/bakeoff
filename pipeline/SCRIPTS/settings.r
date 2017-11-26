##########################################################################################
# SETTINGS
##########################################################################################

REPs<-100
nsamp <- REPs

# basic working directories
##########################################################################################
WD<-"~/OneDrive - University of Helsinki/bakeoff/pipeline/"
#WD <- "..." # the location of the pipeline
#WD2 <- "..." # the location of the results produced with HMSC (see 'HMSCvignette')

# data directory
DD <- paste(WD,"DATA/",sep="")

# model fitting scripts directory
MD <- paste(WD,"MODELS/",sep="")

# model fits directory
FD <- paste(WD,"FITS/",sep="")

# prediction scripts directory
PD <- paste(WD,"PREDICT/",sep="")

# predictions directory
PD2 <- paste(WD,"PREDICTIONS/",sep="")

# scripts directory for prev., rich, co-occ, etc. calculations
RD <- paste(WD,"RESULTS/",sep="")

# predictions  directory for prev., rich, co-occ, etc. calculations  (desktops)
RD2 <- paste(WD,"RESULTS2/",sep="")

# final results directory
RDfinal <- paste(WD,"RESULTS_final/",sep="")

DIRS<-list(DD,MD,FD,PD,PD2,RD,RD2,RDfinal)


# settings for parallel looping
##########################################################################################

# MAC
library('doMC')
crs<-4
registerDoMC(cores=crs)

# PC
#library('doParallel')
#crs<-4
#registerDoParallel(cl=makeCluster(crs))


# read data
##########################################################################################
readdata<-paste(WD,"read.data.r",sep="")

# save objects
##########################################################################################
saveobjs<-c('d','set_no','SETT','readdata','comTimes','saveobjs')

# data sets
##########################################################################################
Sets <- c("butterfly","bird","diatom","tree","plant","fungi")

# models
##########################################################################################
mod_names <- list("GAM","GAM_spat1","GAM_spat2",
				"GLM","GLM_PQL","GLM_PQL_spat","SPAMM","SPAMM_spat",
				"MVABUND",
				"MRTS","MRTS_spat",
				"GNN",
				"RF",
				"BRT",
				"SVM",
				"MARS-COMM","MARS-INT",
				"MVRF",
				"GJAM",
				"SAM",
				"MISTN",
				"HMSC_ss","HMSC", "HMSC_re", "HMSC_spat")
				
nmodels<-length(mod_names)
models <- 1:nmodels


# predictions
##########################################################################################

preds_I <- c("gam_PAs_I.r",
			"gam_PAs_I_spat1.r",
			"gam_PAs_I_spat2.r",
			"glm_PAs_I.r",
			"glmmPQL_PAs_I.r",  
			"glmmPQL_spat_PAs_I.r",  
			"spaMM_PAs_I.r",  
			"spaMMspat_PAs_I.r",  
			"manyglm_PAs_I.r",
			"mrt_PAs_I.r", 
			"mrt_PAs_I_spat.r",
			"gnn_PAs_I.r",
			"rf_PAs_I.r",
			"brt_PAs_I.r",
			"svm_PAs_I.r",
			"mars_PAs_I.r",
			"mars_int_PAs_I.r",
			"mvrf_PAs_I.r",
			"gjam_PAs_I.r", 
			"sam_PAs.r",
			"mstnt_PAs_I.r",
			"ss_hmsc_PAs_I_1.r",
			"hmsc_PAs_I_1.r",
			"hmsc_PAs_I_2.r",
			"hmsc_PAs_I_3.r")

preds_E <- c("gam_PAs_E.r",
			"gam_PAs_E_spat1.r",
			"gam_PAs_E_spat2.r",
			"glm_PAs_E.r",
			"glmmPQL_PAs_E.r",  
			"glmmPQL_spat_PAs_E.r",  
			"spaMM_PAs_E.r",  
			"spaMMspat_PAs_E.r",  
			"manyglm_PAs_E.r",
			"mrt_PAs_E.r", 
			"mrt_PAs_E_spat.r", 
			"gnn_PAs_E.r", 
			"rf_PAs_E.r", 
			"brt_PAs_E.r", 
			"svm_PAs_E.r", 
			"mars_PAs_E.r", 
			"mars_int_PAs_E.r",
			"mvrf_PAs_E.r",
			"gjam_PAs_E.r", 
			"sam_PAs_e.r", 
			"mstnt_PAs_E.r", 
			"ss_hmsc_PAs_E_1.r",			
			"hmsc_PAs_E_1.r", 
			"hmsc_PAs_E_2.r", 
			"hmsc_PAs_E_3.r")

nmodels <- length(preds_I)

pred_comms_I <- list()
pred_comms_E <- list()

##########################################################################################

