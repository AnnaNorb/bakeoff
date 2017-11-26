##########################################################################################
# SPECIES ARCHETYPE MODELS
##########################################################################################

require(mvabund)
source(paste(MD,"coordinSAMsv2.R",sep=""))

##########################################################################################

for (j in 1:3) {

	X <- as.matrix(x_train[[j]][,-1])
	Xs <- cbind(X,as.matrix(s_train[[j]]))
	Y <- as.matrix(y_train[[j]])

	K.range <- 3:18
	all.ics <- numeric(length(K.range))
	names(all.ics) <- K.range
	all.ics2 <- numeric(length(K.range))
	names(all.ics2) <- K.range
	sams <- list(ics = rep(Inf,3))
	sams2 <- list(ics = rep(Inf,3))


	if (j==1) { sT<-Sys.time() }
	
	for (k in 1:length(K.range)) {
		message("Onto K = ", K.range[k])

		dosams <- try(psams.coord(X = X, y = Y, family = "binomial", K = K.range[k], restarts = 10, max.steps = 100, trace = TRUE), silent = TRUE)
		## Switch to trace = FALSE if it is spitting out too much boring stuff
	
		if(!inherits(dosams,"try-error")) { 
		if(dosams$ics[2] < sams$ics[2]) { sams <- dosams } 
			all.ics[k] <- dosams$ics[2]
			
		}
	}

	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	
	save(sams, file=paste(FD,set_no,"/sams_",j,"_",dataN[sz],".RData",sep=""))
	noSAMs <- nrow(sams$betas)
	save(noSAMs, file=paste(FD,set_no,"/noSAMs_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_SAM1_",dataN[sz],".RData",sep=""))
	}


	if (j==1) { sT<-Sys.time() }
	
	for (k in 1:length(K.range)) {
		message("Onto K = ", K.range[k])

		dosams2 <- try(psams.coord(X = X, y = Y, family = "binomial", K = K.range[k], restarts = 10, max.steps = 100, trace = TRUE), silent = TRUE)
		## Switch to trace = FALSE if it is spitting out too much boring stuff
	
		if(!inherits(dosams2,"try-error")) { 
		if(dosams2$ics[2] < sams2$ics[2]) { sams2 <- dosams2 } 
			all.ics2[k] <- dosams2$ics[2]
			
		}
	}

	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	
	save(sams2, file=paste(FD,set_no,"/sams2_",j,"_",dataN[sz],".RData",sep=""))
	noSAM2s <- nrow(sams2$betas)
	save(noSAM2s, file=paste(FD,set_no,"/noSAM2s_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_SAM2_",dataN[sz],".RData",sep=""))
	}

}
##########################################################################################
