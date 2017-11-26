##########################################################################################
# BASIC UNIVARIATE GENERALIZED LINEAR MODELS PREDICTIONS
##########################################################################################

for (j in 1:3) {

	load(file=paste(FD,set_no,"/glm_",j,"_",dataN[sz],".RData",sep=""))
	load(file=paste(FD,set_no,"/glm2_",j,"_",dataN[sz],".RData",sep=""))

	nsp <- length(DD_v[[j]])
	nsites <- nrow(DD_v[[j]][[1]])

	glm_PAs <- array(NA,dim=list(nsites,nsp,REPs))
	glm2_PAs <- glm_PAs

	glm_preds<-foreach(i=1:nsp) %dopar% { tryCatch({ predict(glms[[i]], newdata=DD_v[[j]][[i]], type='response')}, 
											error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	glm2_preds<-foreach(i=1:nsp) %dopar% { tryCatch({ predict(glms2[[i]], newdata=DD_v[[j]][[i]], type='response')}, 
											error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

	for (i in 1:nsp) {
		if(is(glm_preds[[i]])[1]=="try-error" | is(glm_preds[[i]])[1]=="NULL"){ glm_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
		if(is(glm2_preds[[i]])[1]=="try-error" | is(glm2_preds[[i]])[1]=="NULL"){ glm2_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
		}

	glm_preds<-simplify2array(glm_preds)
	glm2_preds<-simplify2array(glm2_preds)

	for (n in 1:REPs) {
		glm_PAs[,,n]<-rbinom(glm_preds,1,glm_preds)
		glm2_PAs[,,n]<-rbinom(glm2_preds,1,glm2_preds)
		}

	save(glm_PAs, file=paste(PD2,set_no,"/glm_PAs_",j,"_",dataN[sz],".RData",sep=""))
	save(glm2_PAs, file=paste(PD2,set_no,"/glm2_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(glms)
	rm(glms2)
	rm(glm_preds)
	rm(glm2_preds)
	rm(glm_PAs)
	rm(glm2_PAs)
	gc()

	}

##########################################################################################
