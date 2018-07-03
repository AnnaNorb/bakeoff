##########################################################################################
# PATHS, ASSIGNMENTS, ETC.
##########################################################################################

dataN<-c(150,300,600)			# data sizes
REPs<-100						# no. of prediction replicates

Sets <- c("birds","butterfly","plant","trees","vegetation")
betaInds<-c("sim", "nest", "sor")


# basic working directories
##########################################################################################
WD <- file.path(pth,"bakeoff","pipeline")

# scripts directory
SD <- file.path(WD,"SCRIPTS")

# data directory
DD <- file.path(WD,"DATA")

# model fitting scripts directory
MD <- file.path(WD,"MODELS")

# model fits directory
FD <- file.path(WD,"FITS")

# model fits directory 2
FD2 <- file.path(WD,"FITS2")

# prediction scripts directory
PD <- file.path(WD,"PREDICT")

# predictions directory
PD2 <- file.path(WD,"PREDICTIONS")

# scripts directory for prev., rich, co-occ, etc. calculations
RD <- file.path(WD,"RESULTS")

# predictions  directory for prev., rich, co-occ, etc. calculations  (desktops)
RD2 <- file.path(WD,"RESULTS2")

# final results directory
RDfinal <- file.path(WD,"RESULTS_final")

DIRS<-c(WD,DD,MD,FD,PD,PD2,RD,RD2,RDfinal)

# read data
##########################################################################################
readdata<-file.path(SD,"read.data.r")

# fit models
##########################################################################################
fitmodels<-file.path(SD,"pipe","fit_models.r")

# make predictions
##########################################################################################
makepreds<-file.path(SD,"pipe","pred.r")

# modify predictions, calculate performance measures, computation times
##########################################################################################
modpredsFolder<-file.path(SD,"pipe","modify_preds")
modpreds<-file.path(SD,"pipe","mod_preds.r")

pms<-file.path(SD,"pipe","pms.r")
pms_tbl<-file.path(SD,"pipe","pms_tbl.r")
pms_comb<-file.path(SD,"pipe","pms_comb.r")
pms_plot<-file.path(SD,"pipe","pms_plot.r")

compt_times<-file.path(SD,"pipe","compt_times.r")

# save objects
##########################################################################################
saveobjs<-c("sz","d","set_no","REPs","SETT","readdata","fitmodels","makepreds",
			"modpredsFolder","modpreds","dataN","saveobjs","Sets","betaInds",
			"pth","WD","SD","DD","MD","FD","PD","PD2","RD","RD2","RDfinal","MCMC2")
saveobjs2<-c(saveobjs,"PMs","PMS","opts","ENS","PRV")

# models
##########################################################################################
mod_names <- list("GAM1","GAM2",
				"GLM1","GLM8",
				"GLM2","GLM3",
				"GLM6",
				"GLM9",
				"MRTS1",
				"GNN1",
				"RF1",
				"BRT1",
				"SVM1",
				"MARS1","MARS2",
				"GJAM1",
				"SAM1",
				"MISTN1",
				"GLM7","BORAL1",
				"BC1","BC2",
				"GLM4","GLM5",
				"HMSC1","HMSC2","HMSC3")
mod_names2 <- c(rep("GAM",2),
				rep("GLM",6),
				"MRTS",
				"GNN",
				"RF",
				"BRT",
				"SVM",
				rep("MARS",2),
				"GJAM",
				"SAM",
				"MISTN",
				"GLM",
				"BORAL",
				rep("BC",2),
				rep("GLM",2),
				rep("HMSC",3))
mod_names3 <- c("GAM","GAMspat1",
				"GLM1","GLM1b",
				"GLMPQL1","GLMPQLspat1",
				"MVABUND1",
				"TRAITGLM1",
				"MRTS1",
				"GNN1",
				"RF1",
				"BRT1",
				"SVM1",
				"MARS1","MARS2",
				"GJAM1",
				"SAM1",
				"MISTN1",
				"BORAL1","BORAL2",
				"BC1","BC2",
				"hmsc1","hmsc2",
				"hmsc1","hmsc2","hmsc3")
cbind(mod_names,mod_names2,mod_names3)
nmodels<-length(mod_names)
models <- 1:nmodels
nfrmwrks<-length(unique(mod_names2))

# predictions
##########################################################################################
pred_names	<-	list("gam1_PAs_","gam_spat1_PAs_",
				"glm1_PAs_","glm1b_PAs_",
				"glmmPQL1_PAs_","glmmPQLspat1_PAs_",
				"manyglm1_PAs_",
				"traitglm1_PAs_",
				"mrt1_PAs_",
				"gnn1_PAs_",
				"rf1_PAs_",
				"brt1_PAs_",
				"svm1_PAs_",
				"mars1_PAs_","mars2_PAs_",
				"gjam1_PAs_",
				"sam1_PAs_",
				"mstnt1_PAs_",
				"boral1_PAs_","boral2_PAs_",
				"bc1_PAs_","bc2_PAs_",
				"ss_hmsc1_PAs_","ss_hmsc2_PAs_",
				"hmsc1_PAs_","hmsc2_PAs_","hmsc3_PAs_")
names(pred_names)<-mod_names

print(cbind(mod_names,mod_names2,mod_names3,pred_names))

##########################################################################################

if (length(mod_names)!=length(pred_names)) {
	stop("The vector of rediction objects and the vector of prediction names are of different size")
	}

# MODEL FEATURES
##########################################################################################
feats<-read.csv2(file.path(DD,"feats.csv"),header=T)
rownames(feats)<-feats[,1]
feats<-feats[unlist(mod_names),]

PMnames		<-	c("accuracy1","discrimination1","sharpness1","calibration1",
				"accuracy2site","accuracy3beta1","accuracy3beta2","accuracy3beta3",
				"discrimination2site","discrimination3beta1","discrimination3beta2","discrimination3beta3",
				"sharpness2site","sharpness3beta1","sharpness3beta2","sharpness3beta3",
				"calibration2site","calibration3beta1","calibration3beta2","calibration3beta3")
minTomaxBest <- c("accuracy1","sharpness1","calibration1",
				"accuracy2site","accuracy3beta1","accuracy3beta2","accuracy3beta3",
				"sharpness2site","sharpness3beta1","sharpness3beta2","sharpness3beta3",
				"calibration2site","calibration3beta1","calibration3beta2","calibration3beta3")

minIsBest1<-matrix(0,nrow=length(PMnames))
rownames(minIsBest1)<-PMnames
minIsBest1[minTomaxBest,]<-1
minIsBest1<-cbind(rownames(minIsBest1),minIsBest1)
if (!file.exists(file.path(RDfinal,"minIsBest1.csv"))) {
	write.table(minIsBest1,file=file.path(RDfinal,"minIsBest1.csv"),sep=",",row.names=F,col.names=F)					
}
##########################################################################################
