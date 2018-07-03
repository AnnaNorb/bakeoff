##########################################################################################
# DIRECTORIES
##########################################################################################

dir.create(PD2)
dir.create(RD)
for (set in Sets) {
	dir.create(paste(RD,set,sep=""))
	}
dir.create(RDfinal)
dir.create(paste(RDfinal,"150/",sep=""))
dir.create(paste(RDfinal,"300/",sep=""))
dir.create(paste(RDfinal,"150/meta_analysis",sep=""))
dir.create(paste(RDfinal,"300/meta_analysis",sep=""))

##########################################################################################
