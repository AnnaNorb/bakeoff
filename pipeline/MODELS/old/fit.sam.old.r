# FITTING SAMS TO FREDNEWDATA, FOR BAKEOFF
##########################################################################################

source(paste(MD,"sam/source/coordinSAMsv2.R",sep=""))

##########################################################################################

X <- as.matrix(X_I_train[,-1])
Y <- as.matrix(y_I_train)

# interpolation
##########################################################################################

K.range <- 3:18
all.ics <- numeric(length(K.range))
names(all.ics) <- K.range
sam_I <- list(ics = rep(Inf,3))

sT<-Sys.time()

for(k in 1:length(K.range)) {
	message("Onto K = ", K.range[k])

	dosams <- try(psams.coord(X = X, y = Y, family = "binomial", K = K.range[k], restarts = 10, max.steps = 100, trace = TRUE), silent = TRUE)
	## Switch to trace = FALSE if it is spitting out too much boring stuff
	
	if(!inherits(dosams,"try-error")) { 
		if(dosams$ics[2] < sam_I$ics[2]) { sam_I <- dosams } 
		all.ics[k] <- dosams$ics[2]
		}
}

eT<-Sys.time()
comTimes$SAM<-eT-sT
	
##########################################################################################

save(sam_I, file=paste(FD,set_no,"/sam_I.r",sep=""))

##########################################################################################

# extrapolation
##########################################################################################

X <- as.matrix(X_E_train[,-1])
Y <- as.matrix(y_E_train)

K.range <- 3:18
all.ics <- numeric(length(K.range))
names(all.ics) <- K.range
sam_E <- list(ics = rep(Inf,3))
for(k in 1:length(K.range)) {
	message("Onto K = ", K.range[k])

	dosams <- try(psams.coord(X = X, y = Y, family = "binomial", K = K.range[k], restarts = 10, max.steps = 100, trace = TRUE), silent = TRUE)
	## Switch to trace = FALSE if it is spitting out too much boring stuff
	
	if(!inherits(dosams,"try-error")) { 
		if(dosams$ics[2] < sam_E$ics[2]) { sam_E <- dosams } 
		all.ics[k] <- dosams$ics[2]
		}
	}

##########################################################################################

save(sam_E, file=paste(FD,set_no,"/sam_E.r",sep=""))

##########################################################################################


