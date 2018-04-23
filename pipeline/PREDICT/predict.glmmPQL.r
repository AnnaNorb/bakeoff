##########################################################################################
# UNIVARIATE GENERALIZED LINEAR MODELS PREDICTIONS
# with multivariate normal random effects, using Penalized Quasi-Likelihood
##########################################################################################

require(nlme)
require(MASS)

##########################################################################################
for (j in 1:3) {

	nsp <- length(DD_v[[j]])
	nsites <- nrow(DD_v[[j]][[1]])

	dada<-lapply(DD_v[[j]],cbind,1:nrow(DD_v[[j]][[1]]))

		for (i in 1:nsp) { colnames(dada[[i]])[ncol(dada[[i]])] <- "ID" }

			glmmPQL1_PAs <- array(NA,dim=list(nsites,nsp,REPs))
			glmmPQLspat1_PAs <- glmmPQL1_PAs
			
			load(file=paste(FD,set_no,"/","glmmpql1_",j,"_",dataN[sz],".RData",sep=""))
			load(file=paste(FD,set_no,"/glmmpql_spat1_",j,"_",dataN[sz],".RData",sep=""))

			glmmPQL1_preds	<- foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ predict(glmmpql1[[i]], newdata=dada[[i]], level=0, type='response')}, 
																						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
			glmmPQL_spat1_preds	<- foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ predict(glmmpql_spat1[[i]], newdata=dada[[i]], level=0, type='response')},
																							error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

		for (i in 1:nsp) {
			if (is(glmmPQL1_preds[[i]])[1]=="try-error" | is(glmmPQL1_preds[[i]])[1]=="NULL"){ glmmPQL1_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
			if (is(glmmPQL_spat1_preds[[i]])[1]=="try-error" | is(glmmPQL_spat1_preds[[i]])[1]=="NULL"){ glmmPQL_spat1_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
			}

		glmmPQL1_preds	<- simplify2array(glmmPQL1_preds)
		glmmPQL_spat1_preds <- simplify2array(glmmPQL_spat1_preds)

		for (n in 1:REPs) {
			glmmPQL1_PAs[,,n]<-rbinom(glmmPQL1_preds,1,glmmPQL1_preds)
			glmmPQLspat1_PAs[,,n]<-rbinom(glmmPQL_spat1_preds,1,glmmPQL_spat1_preds)
		}

		save(glmmPQL1_PAs, file=paste(PD2,set_no,"/glmmPQL1_PAs_",j,"_",dataN[sz],".RData",sep=""))
		save(glmmPQLspat1_PAs, file=paste(PD2,set_no,"/glmmPQLspat1_PAs_",j,"_",dataN[sz],".RData",sep=""))

		rm(glmmPQL1_preds)
		rm(glmmPQL_spat1_preds)
		rm(glmmPQL1_PAs)
		rm(glmmPQLspat1_PAs)
		gc()

	}
	
##########################################################################################

