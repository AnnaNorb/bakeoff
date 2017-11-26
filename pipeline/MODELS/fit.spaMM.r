##########################################################################################
# UNIVARIATE GENERALIZED LINEAR MIXED MODELS
# with given correlation matrix
##########################################################################################

require(spaMM)

##########################################################################################

if (set_no=="birds" | set_no=="butterfly" | set_no=="plant") { 
	form <- as.formula(sp~IC+V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2)
	form2 <- as.formula(sp~IC+V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2+Rand1+(Rand1^2)+Rand2+(Rand2^2))
	form_spat <- as.formula(sp~IC+V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2+Matern(1|Rand1+Rand2))
}
if (set_no=="trees") { 
	form <- as.formula(sp~IC+V1+V2+V3+V1_2+V2_2+V3_2)
	form2 <- as.formula(sp~IC+V1+V2+V3+V1_2+V2_2+V3_2+Rand1+(Rand1^2)+Rand2+(Rand2^2))
	form_spat <- as.formula(sp~IC+V1+V2+V3+V1_2+V2_2+V3_2+Matern(1|Rand1+Rand2))
}
if (set_no=="vegetation") { 
	form <- as.formula(sp~IC+V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
	form2 <- as.formula(sp~IC+V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2+Rand1+(Rand1^2)+Rand2+(Rand2^2))
	form_spat <- as.formula(sp~IC+V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2+Matern(1|Rand1+Rand2))
}

for (j in 1:3) {

	nsp <- length(DD_t[[j]])
	spaMM <- list()
	spaMM_spat <- list()

	if (j==1) { sT<-Sys.time() }
	spaMM	<-	foreach	(i=1:nsp, .packages='spaMM') %dopar% { tryCatch({ HLfit(form, family=binomial(link = "probit"), data=DD_t[[j]][[i]])},
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(spaMM, file=paste(FD,set_no,"/spaMM_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_SPAMM_",dataN[sz],".RData",sep=""))
	}

	if (j==1) { sT<-Sys.time() }
	spaMM2	<-	foreach	(i=1:nsp, .packages='spaMM') %dopar% { tryCatch({ HLfit(form2, family=binomial(link = "probit"), data=DD_t[[j]][[i]])},
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}
	save(spaMM2, file=paste(FD,set_no,"/spaMM2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_SPAMM2_",dataN[sz],".RData",sep=""))
	}
	
# 	if (j==1) { sT<-Sys.time() }
# 	spaMM_spat	<-	foreach	(i=1:nsp, .packages='spaMM') %dopar% { tryCatch({ corrHLfit(form_spat, family=binomial(link = "probit"), data=DD_t[[j]][[i]])},
# 								error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
# 	 if (j==1) {
# 		eT<-Sys.time()
# 		comTimes<-eT-sT
# 		}
# 	save(spaMM_spat, file=paste(FD,set_no,"/spaMM_spat_",j,"_",dataN[sz],".RData",sep=""))
# 	if (j==1) {
# 	save(comTimes, file=paste(FD,set_no,"/comTimes_SPAMMspat_",dataN[sz],".RData",sep=""))
# 	}
	
	}

##########################################################################################

