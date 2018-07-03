##########################################################################################
# SPECIES ARCHETYPE MODELS
##########################################################################################

require(mvabund)
source(paste(MD,"coordinSAMsv2.R",sep=""))

##########################################################################################

for (j in 1:3) {

	X <- as.matrix(x_train[[j]][,-1])
	Y <- as.matrix(y_train[[j]])

	K.range <- 3:13
	all.ics <- numeric(length(K.range))
	names(all.ics) <- K.range
	sams1 <- list(ics = rep(Inf,3))


	if (j==1) { sT<-Sys.time() }
	
	for (k in 1:length(K.range)) {
		message("Onto K = ", K.range[k])

		dosams <- try(psams.coord(X=X, y=Y, family="binomial", K=K.range[k], restarts=10, max.steps=100, trace=FALSE), silent=TRUE)
	
		if(!inherits(dosams,"try-error")) { 
		if(dosams$ics[2] < sams1$ics[2]) { sams1 <- dosams } 
			all.ics[k] <- dosams$ics[2]
			
		}
	}

	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	
	save(sams1, file=file.path(FD,set_no,paste("sams1_",j,"_",dataN[sz],".RData",sep="")))
	noSAMs1 <- nrow(sams1$betas)
	save(noSAMs1, file=file.path(FD,set_no,paste("noSAMs1_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_SAM1_",dataN[sz],".RData",sep="")))
	}
}
##########################################################################################
