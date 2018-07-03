##########################################################################################
# DIRECTORIES
##########################################################################################

dir.create(PD2)
dir.create(RD2)
for (set in Sets) {
	dir.create(paste(RD2,set,sep=""))
	}
dir.create(RDfinal)
dir.create(paste(RDfinal,"150/",sep=""))
dir.create(paste(RDfinal,"300/",sep=""))
dir.create(paste(RDfinal,"600/",sep=""))
dir.create(paste(RDfinal,"150/meta_analysis",sep=""))
dir.create(paste(RDfinal,"300/meta_analysis",sep=""))
dir.create(paste(RDfinal,"600/meta_analysis",sep=""))

##########################################################################################
