##########################################################################################
# BASIC UNIVARIATE GENERALIZED LINEAR MODELS PREDICTIONS
##########################################################################################

for (j in 1:3) {

	load(file=paste(FD,set_no,"/glm1_",j,"_",dataN[sz],".RData",sep=""))

	nsp <- length(DD_v[[j]])
	nsites <- nrow(DD_v[[j]][[1]])

	glm1_PAs <- array(NA,dim=list(nsites,nsp,REPs))

	glm1_preds<-foreach(i=1:nsp) %dopar% { tryCatch({ predict(glms[[i]], newdata=DD_v[[j]][[i]], type='response')}, 
											error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

	for (i in 1:nsp) {
		if(is(glm1_preds[[i]])[1]=="try-error" | is(glm1_preds[[i]])[1]=="NULL"){ glm1_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
	}

	glm1_preds<-simplify2array(glm1_preds)

	for (n in 1:REPs) {
		glm1_PAs[,,n]<-rbinom(glm1_preds,1,glm1_preds)
	}

	save(glm1_PAs, file=paste(PD2,set_no,"/glm1_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(glms1)
	rm(glm1_preds)
	rm(glm1_PAs)
	gc()

}

##########################################################################################
