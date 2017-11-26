##########################################################################################
# PATHS, ASSIGNMENTS, ETC.
##########################################################################################

dataN<-c(300,600,'custom')	# data sizes
REPs<-100					# no. of pred replicates

# root path
##########################################################################################
info<-Sys.info()

if (info[4]=="DH2-BIOTI14" | info[4]=="DH2-BIOTI15"){ 
	pth<-"D:/HY-data/NORBERG/OneDrive - University of Helsinki/" 
	} 
if (info[5]=="x86_64"){
	pth<-"~/OneDrive - University of Helsinki/" 
	}

# basic working directories
##########################################################################################

WD <- paste(pth,"bakeoff/pipeline/",sep="")
WD2 <- paste(pth,"HMSC-ELE/bakeoff/",sep="")	# huom laita tää automaattiseksi github versioon
WD3 <- paste(pth,"bakeoff/ms/",sep="")			# huom laita tää automaattiseksi github versioon

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

if (info[4]=="DH1-MRG44" | info[4]=="DH2-BIOTI15" | info[4]=="DH2-BIOTI14"){
	require(doParallel)
	crs<-16
	registerDoParallel(cl=makeCluster(crs))
}
if (info[5]=="x86_64"){
	require(doMC)
	crs<-2
	registerDoMC(cores=crs)
}


# read data
##########################################################################################
readdata<-paste(WD,"SCRIPTS/read.data.r",sep="")
##########################################################################################

# calculate occ probs, sp rich, beta indices
##########################################################################################
modpreds<-paste(WD,"SCRIPTS/modify.preds.r",sep="")
##########################################################################################

# save objects
##########################################################################################
saveobjs<-c('sz','d','set_no','REPs','SETT','readdata','comTimes','dataN','saveobjs')

#install.packages(c('sdm','devtools','earth','yaImpute','randomForest','e1071','vegan','glmnet','gbm','dismo','mvabund','caret'))
#library("devtools")
#install.packages("registerDoMC", repos="http://cran.at.r-project.org/")
#install.packages(paste(MD,"mrts/mvpart_1.6-2.zip",sep=""))
#install.packages(paste(MD,"mistnet/mistnet2_0.2.0.zip",sep=""),dependencies=TRUE)


# data sets
##########################################################################################
Sets <- c("birds","butterfly","plant","trees","vegetation")

# models
mod_names <- list("GAM","GAM2","GAMspat1","GAMspat2",
				"GLM","GLM2",
				"GLMPQL","GLMPQL2","GLMPQLspat",
				"SPAMM","SPAMM2",
				#"SPAMMspat",
				"MVABUND1", "MVABUND2","MVABUND3",
				"MRTS1","MRTS2",
				"GNN1","GNN2",
				"RF", "RF2",
				"BRT","BRT2",
				"SVM","SVM2",
				"MARS_COMM1","MARS_COMM2","MARS_INT1","MARS_INT2",
				"MVRF1", "MVRF2",
				"GJAM1","GJAM2",
				"SAM","SAM2",
				"MISTN1","MISTN2",
				"BORAL1","BORAL2","BORAL3","BORAL4",
				#"SPBAYESspat",
				"BC1","BC2",
				"ssHMSC","ssHMSC2","HMSC1", "HMSC2", "HMSC3","HMSC4")
				
nmodels<-length(mod_names)
models <- 1:nmodels


# predictions
##########################################################################################

pred_names	<-	list("gam_PAs_","gam2_PAs_","gam_spat1_PAs_","gam_spat2_PAs_",
				"glm_PAs_","glm2_PAs_",
				"glmmPQL_PAs_","glmmPQL2_PAs_","glmmPQLspat_PAs_",
				"spaMM_PAs_","spaMM2_PAs_",
				#"spaMMspat_PAs_",
				"traitglm1_PAs_","traitglm2_PAs_","traitglm3_PAs_",
				"mrt_PAs_","mrt2_PAs_",
				"gnn_PAs_","gnn2_PAs_",
				"rf_PAs_","rf2_PAs_",
				"brt_PAs_","brt2_PAs_",
				"svm_PAs_","svm2_PAs_",
				"mars_PAs_","mars2_PAs_","mars_int_PAs_","mars_int2_PAs_",
				"mvrf_PAs_","mvrf2_PAs_",
				"gjam_PAs_","gjam2_PAs_",
				"sam_PAs_","sam2_PAs_",
				"mstnt_PAs_","mstnt2_PAs_",
				"boral1_PAs_","boral2_PAs_","boral3_PAs_","boral4_PAs_",
				#"spBspat_",
				"bc1_PAs_","bc2_PAs_",
				"ss_hmsc1_PAs_","ss_hmsc2_PAs_",
				"hmsc1_PAs_","hmsc2_PAs_","hmsc3_PAs_","hmsc4_PAs_")

pred_comms <- list()


##########################################################################################

cbind(mod_names,pred_names)

##########################################################################################
