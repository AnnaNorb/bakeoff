##########################################################################################
# UNIVARIATE GENERALIZED LINEAR MODELS
# with multivariate normal random effects, using Penalized Quasi-Likelihood
##########################################################################################

require(nlme)
require(MASS)

##########################################################################################

if (set_no=="birds" | set_no=="butterfly" | set_no=="plant") { 
	form <- as.formula(sp~V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2)
	form2 <- as.formula(sp~V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2+Rand1+(Rand1^2)+Rand2+(Rand2^2))
}
if (set_no=="trees") { 
	form <- as.formula(sp~V1+V2+V3+V1_2+V2_2+V3_2)
	form2 <- as.formula(sp~V1+V2+V3+V1_2+V2_2+V3_2+Rand1+(Rand1^2)+Rand2+(Rand2^2))
}
if (set_no=="vegetation") { 
	form <- as.formula(sp~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
	form2 <- as.formula(sp~V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2+Rand1+(Rand1^2)+Rand2+(Rand2^2))
}

for (j in 1:3) {

	nsp <- length(DD_t[[j]])
	
	dada<-lapply(DD_t[[j]],cbind,1:nrow(DD_t[[j]][[1]]))
	
	for (i in 1:nsp) { colnames(dada[[i]])[ncol(dada[[i]])] <- "ID" }

	glmmpql <- list()
	glmmpql2 <- list()
	glmmpql_spat <- list()

	if (j==1) { sT<-Sys.time() }
	glmmpql	<-	foreach	(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({ 
				glmmPQL(form, random= ~1|ID, family=binomial(link = "probit"), data=dada[[i]]) },
				error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
  	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(glmmpql, file=paste(FD,set_no,"/","glmmpql_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GLMPQL_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
	glmmpql2	<-	foreach	(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({
					glmmPQL(form2, random= ~1|ID, family=binomial(link = "probit"), data=dada[[i]])},
					error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
  	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(glmmpql2, file=paste(FD,set_no,"/","glmmpql2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GLMPQL2_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
	glmmpql_spat	<-	foreach(i=1:nsp, .packages=c("MASS","nlme")) %dopar% { tryCatch({
						glmmPQL(form, random= ~ 1|ID, 
						correlation=Initialize(corSpatial(1,form = ~Rand1+Rand2, type = "exponential"), data = dada[[i]]), 
						family=binomial(link = "probit"), data=dada[[i]])},
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
  	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(glmmpql_spat, file=paste(FD,set_no,"/glmmpql_spat_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GLMPQLspat_",dataN[sz],".RData",sep=""))
	}

}

##########################################################################################
