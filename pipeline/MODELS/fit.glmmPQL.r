##########################################################################################
# UNIVARIATE GENERALIZED LINEAR MODELS using Penalized Quasi-Likelihood
##########################################################################################

require(nlme)
require(MASS)

##########################################################################################

if (set_no=="birds" | set_no=="butterfly" | set_no=="plant") { 
	form <- as.formula(sp~V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2)
}
if (set_no=="trees") { 
	form <- as.formula(sp~V1+V2+V3+V1_2+V2_2+V3_2)
}
if (set_no=="vegetation") { 
	form <- as.formula(sp~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
}

##########################################################################################

for (j in 1:3) {

	nsp <- length(DD_t[[j]])
	
	dada<-lapply(DD_t[[j]],cbind,1:nrow(DD_t[[j]][[1]]))
	
	for (i in 1:nsp) { colnames(dada[[i]])[ncol(dada[[i]])] <- "ID" }

	glmmpql1 <- list()
	glmmpql_spat1 <- list()

	if (j==1) { sT<-Sys.time() }
	glmmpql1	<-	foreach	(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ 
				glmmPQL(form, random= ~1|ID, family=binomial(link = "probit"), data=dada[[i]]) },
				error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
  	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(glmmpql1, file=paste(FD,set_no,"/","glmmpql1_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
		save(comTimes, file=paste(FD,set_no,"/comTimes_GLMPQL1_",dataN[sz],".RData",sep=""))
		rm(comTimes)
	}

	if (j==1) { sT<-Sys.time() }
	glmmpql_spat1	<-	foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({
						glmmPQL(form, random= ~ 1|ID, 
						correlation=Initialize(corSpatial(1,form = ~Rand1+Rand2, type = "exponential"), data = dada[[i]]), 
						family=binomial(link = "probit"), data=dada[[i]])},
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
  	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(glmmpql_spat1, file=paste(FD,set_no,"/glmmpql_spat1_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GLMPQLspat1_",dataN[sz],".RData",sep=""))
	rm(comTimes)
	}

}

##########################################################################################
