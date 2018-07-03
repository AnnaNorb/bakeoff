##########################################################################################
# MODIFY PREDICTIONS
##########################################################################################

rm(list=ls()[!(ls() %in% c("OS","pth","SETT","ENS","PRV"))]); gc(); source(SETT); setwd(WD)

modPredScrs <- c(file.path(modpredsFolder,"sp_occ_probs.r"),
				file.path(modpredsFolder,"sp_rich_site.r"),
				file.path(modpredsFolder,"beta_inds.r"))		

for (e in 1:length(ENS)) {
	for (p in 1:length(PRV)) {
		opts<-list(modelEnsemble=ENS[[e]],prevaleceThreshold=PRV[[p]])		
		for (sz in 1:2) {
			for (d in 1:length(Sets)) {
				set_no <- Sets[d]; source(readdata)				
				foreach	(i=1:length(modPredScrs)) %dopar% { source(modPredScrs[i]) } 
			}
		}
	}
}

##########################################################################################
