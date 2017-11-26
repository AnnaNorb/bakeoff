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

			glmmPQL_PAs <- array(NA,dim=list(nsites,nsp,REPs))
			glmmPQL2_PAs <- glmmPQL_PAs
			glmmPQLspat_PAs <- glmmPQL_PAs
			
			load(file=paste(FD,set_no,"/","glmmpql_",j,"_",dataN[sz],".RData",sep=""))
			load(file=paste(FD,set_no,"/","glmmpql2_",j,"_",dataN[sz],".RData",sep=""))
			load(file=paste(FD,set_no,"/glmmpql_spat_",j,"_",dataN[sz],".RData",sep=""))

			glmmPQL_preds	<- foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ predict(glmmpql[[i]], newdata=dada[[i]], type='response')}, 
																						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
			glmmPQL2_preds	<- foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ predict(glmmpql2[[i]], newdata=dada[[i]], type='response')},
																							error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
			glmmPQL_spat_preds	<- foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ predict(glmmpql_spat[[i]], newdata=dada[[i]], type='response')},
																							error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

		for (i in 1:nsp) {
			if (is(glmmPQL_preds[[i]])[1]=="try-error" | is(glmmPQL_preds[[i]])[1]=="NULL"){ glmmPQL_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
			if  (is(glmmPQL2_preds[[i]])[1]=="try-error" | is(glmmPQL2_preds[[i]])[1]=="NULL"){ glmmPQL2_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
			if(is(glmmPQL_spat_preds[[i]])[1]=="try-error" | is(glmmPQL_spat_preds[[i]])[1]=="NULL"){ glmmPQL_spat_preds[[i]] <-  rep(mean(DD_t[[j]][[i]][,1]),times=nsites) }
			}

		glmmPQL_preds	<- simplify2array(glmmPQL_preds)
		glmmPQL2_preds <- simplify2array(glmmPQL2_preds)
		glmmPQL_spat_preds <- simplify2array(glmmPQL_spat_preds)

		for (n in 1:REPs) {
			glmmPQL_PAs[,,n]<-rbinom(glmmPQL_preds,1,glmmPQL_preds)
			glmmPQL2_PAs[,,n]<-rbinom(glmmPQL2_preds,1,glmmPQL2_preds)
			glmmPQLspat_PAs[,,n]<-rbinom(glmmPQL_spat_preds,1,glmmPQL_spat_preds)
			}

		rm(glmmPQL_preds)
		rm(glmmPQL2_preds)
		rm(glmmPQL_spat_preds)
		rm(glmmpql)
		rm(glmmpql2)
		rm(glmmpql_spat)

		save(glmmPQL_PAs, file=paste(PD2,set_no,"/glmmPQL_PAs_",j,"_",dataN[sz],".RData",sep=""))
		save(glmmPQL2_PAs, file=paste(PD2,set_no,"/glmmPQL2_PAs_",j,"_",dataN[sz],".RData",sep=""))
		save(glmmPQLspat_PAs, file=paste(PD2,set_no,"/glmmPQLspat_PAs_",j,"_",dataN[sz],".RData",sep=""))

		rm(glmmPQL_PAs)
		rm(glmmPQL2_PAs)
		rm(glmmPQLspat_PAs)
		gc()

	}
	
##########################################################################################

