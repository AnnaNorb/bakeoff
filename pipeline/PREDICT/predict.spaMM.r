##########################################################################################
# UNIVARIATE GENERALIZED LINEAR MIXED MODELS PREDICTIONS
# with given correlation matrix
##########################################################################################

require(spaMM)

##########################################################################################

for (j in 1:3) {

	nsp <- length(DD_v[[j]])
	nsites <- nrow(DD_v[[j]][[1]])

	load(file=paste(FD,set_no,"/spaMM_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/spaMM2_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/spaMM_spat_",j,"_",dataN[sz],".RData",sep=""))

	spaMM_preds <- foreach (i=1:nsp) %dopar% { tryCatch({ predict(spaMM[[i]], newdata=DD_v[[j]][[i]], type='response')}, 
												error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	spaMM2_preds <- foreach (i=1:nsp) %dopar% { tryCatch({ predict(spaMM2[[i]], newdata=DD_v[[j]][[i]], type='response')}, 
												error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	spaMMspat_preds <- foreach (i=1:nsp) %dopar% { tryCatch({ predict(spaMM_spat[[i]], newdata=DD_v[[j]][[i]], type='response')}, 
													error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }


	for (i in 1:nsp) {
		if(is(spaMM_preds[[i]])[1]=="try-error" | is(spaMM_preds[[i]])[1]=="NULL"){ spaMM_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
		if(is(spaMM2_preds[[i]])[1]=="try-error" | is(spaMM2_preds[[i]])[1]=="NULL"){ spaMM2_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
		if(is(spaMMspat_preds[[i]])[1]=="try-error" | is(spaMMspat_preds[[i]])[1]=="NULL"){ spaMMspat_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
		}

		spaMM_preds<-simplify2array(spaMM_preds)
		spaMM2_preds<-simplify2array(spaMM2_preds)
		spaMMspat_preds<-simplify2array(spaMMspat_predsMM2_preds)

		spaMM_PAs <- array(NA,dim=list(nsites,nsp,REPs))
		spaMM2_PAs <- spaMM_PAs
		spaMMspat_PAs <- spaMM_PAs
		for (n in 1:REPs) {
			spaMM_PAs[,,n]<-rbinom(spaMM_preds,1,spaMM_preds)
			spaMM2_PAs[,,n]<-rbinom(spaMM2_preds,1,spaMM2_preds)
			spaMMspat_PAs[,,n]<-rbinom(spaMMspat_preds,1,spaMMspat_preds)
			}

		rm(spaMM)
		rm(spaMM2)
		rm(spaMM_spat)
		rm(spaMM_preds)
		rm(spaMM2_preds)
		rm(spaMMspat_preds)

		save(spaMM_PAs, file=paste(PD2,set_no,"/spaMM_PAs_",j,"_",dataN[sz],".RData",sep=""))
		save(spaMM2_PAs, file=paste(PD2,set_no,"/spaMM2_PAs_",j,"_",dataN[sz],".RData",sep=""))
		save(spaMMspat_PAs, file=paste(PD2,set_no,"/spaMMspat_PAs_",j,"_",dataN[sz],".RData",sep=""))

		rm(spaMM_PAs)
		rm(spaMM2_PAs)
		rm(spaMMspat_PAs)
	
		gc()
	
	}	

##########################################################################################
