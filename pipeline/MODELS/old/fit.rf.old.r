# fit RANDOM FOREST models
##########################################################################################

library('randomForest')
library('e1071')

##########################################################################################

ntrees<-c(50,500,2000)

# Interpolation
##########################################################################################
nsp <- length(dd_i_t)

dd_i_t_rf<-dd_i_t
for (i in 1:length(dd_i_t)) {
dd_i_t_rf[[i]][,1]<-as.factor(dd_i_t[[i]][,1])
}

rf_I <- list()
sT<-Sys.time()
for (s in 1:nsp) {
	rf_I <- NULL	
	rf_I <-	tryCatch({( randomForest(y=dd_i_t_rf[[s]][,1], x=dd_i_t_rf[[s]][,3:(2+ncovar)],
									importance=TRUE,keep.forest=TRUE,proximity=TRUE,ntree=ntrees[2])[c(1,2,4:11,13:15,17:18)] 
									)  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
	if (is.null(rf_I)!=TRUE) { save(rf_I, file=paste(FD,set_no,"/rfs/rf_I_",s,".r",sep="")) }
	rm(rf_I)
	gc()
}
eT<-Sys.time()
comTimes$RF<-eT-sT


# Extrapolation
##########################################################################################
nsp_e <- length(dd_e_t)

dd_e_t_rf<-dd_e_t
for (i in 1:length(dd_e_t)) {
dd_e_t_rf[[i]][,1]<-as.factor(dd_e_t[[i]][,1])
}

for (s in 1:nsp_e) {
	rf_E <- NULL	
	rf_E <-	tryCatch({( randomForest(y=dd_e_t_rf[[s]][,1], x=dd_e_t_rf[[s]][,3:(2+ncovar)],
									importance=TRUE,keep.forest=TRUE,proximity=TRUE,ntree=ntrees[2])[c(1,2,4:11,13:15,17:18)] 
									)  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
if (is.null(rf_E)!=TRUE) { save(rf_E, file=paste(FD,set_no,"/rfs/rf_E_",s,".r",sep="")) }
rm(rf_E)
gc()
}

##########################################################################################



