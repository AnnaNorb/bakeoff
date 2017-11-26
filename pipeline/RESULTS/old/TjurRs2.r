##########################################################################################
# COMPILING RESULTS
##########################################################################################

TJURS <- array(NA, dim=list(nmodels,ncol(y_valid[[1]]),3))

for (j in 1:3) {		

	for (m in models) {

	load(file=paste(RD2,"Tjurs/",Sets[d],"/tjurs_",mod_names[[m]],"_",j,"_",dataN[sz],".RData",sep=""))
  
	TJURS[m,,j]<-tjurs
	rm(tjurs)
	gc() 	
	
		}  
	}

save(TJURS, file=paste(RDfinal,dataN[sz],"/TJURS_",Sets[d],".RData",sep=""))

##########################################################################################