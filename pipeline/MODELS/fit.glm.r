##########################################################################################
# BASIC UNIVARIATE GENERALIZED LINEAR MODELS
##########################################################################################

if (set_no=="birds" | set_no=="butterfly" | set_no=="plant") { 
	form <- as.formula(sp~1+V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2)
}
if (set_no=="trees") { 
	form <- as.formula(sp~1+V1+V2+V3+V1_2+V2_2+V3_2)
}
if (set_no=="vegetation") { 
	form <- as.formula(sp~1+V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
}


##########################################################################################

for (j in 1:3) {

	nsp <- length(DD_t[[j]])

	if (j==1) { sT<-Sys.time() }
		glms1	<-	foreach	(i=1:nsp) %dopar% { tryCatch({ glm(form, family=binomial(link = "probit"), data=DD_t[[j]][[i]])}, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
	}

	save(glms1, file=paste(FD2,set_no,"/glm1_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
		save(comTimes, file=paste(FD2,set_no,"/comTimes_GLM1_",dataN[sz],".RData",sep=""))
	}
}

##########################################################################################
