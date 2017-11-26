##########################################################################################
# BASIC UNIVARIATE GENERALIZED LINEAR MODELS
##########################################################################################

require("pROC")

##########################################################################################

if (set_no=="birds" | set_no=="butterfly" | set_no=="plant") { 
	form <- as.formula(sp~IC+V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2)
	form2 <- as.formula(sp~IC+V1+V2+V3+V4+V5+V1_2+V2_2+V3_2+V4_2+V5_2+Rand1+(Rand1^2)+Rand2+(Rand2^2))
	}
if (set_no=="trees") { 
	form <- as.formula(sp~IC+V1+V2+V3+V1_2+V2_2+V3_2)
	form2 <- as.formula(sp~IC+V1+V2+V3+V1_2+V2_2+V3_2+Rand1+(Rand1^2)+Rand2+(Rand2^2))
	}
if (set_no=="vegetation") { 
	form <- as.formula(sp~IC+V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2)
	form2 <- as.formula(sp~IC+V1+V2+V3+V4+V1_2+V2_2+V3_2+V4_2+Rand1+(Rand1^2)+Rand2+(Rand2^2))
	}

for (j in 1:3) {

	nsp <- length(DD_t[[j]])

	glms <- list()

	if (j==1) { sT<-Sys.time() }
		bglms	<-	foreach	(i=1:nsp) %dopar% { tryCatch({ glm(form, family=binomial(link = "logit"), data=DD_t[[j]][[i]])}, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}

	qbglms	<-	foreach	(i=1:nsp) %dopar% { tryCatch({ glm(form, family=quasibinomial(link = "logit"), data=DD_t[[j]][[i]])}, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

	for (i in 1:length(bglms)) {
		aucs<-NULL
		tryCatch({ aucs<-c(auc(response=DD_t[[j]][[i]][,1],predict(bglms[[i]],type="response")),
					auc(response=DD_t[[j]][[i]][,1],predict(qbglms[[i]],type="response")))}, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
			if (is.null(aucs)) { 
				aucs<-c(cor(cumsum(predict(bglms[[i]],type="response")), cumsum(DD_t[[j]][[i]][,1])),
					  cor(cumsum(predict(qbglms[[i]],type="response")), cumsum(DD_t[[j]][[i]][,1])))
				}			
			if (aucs[1]>=aucs[2]) { glms[[i]]<-bglms[[i]] }	
			if (aucs[1]<aucs[2]) { glms[[i]]<-qbglms[[i]] }
		}

	save(glms, file=paste(FD,set_no,"/glm_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GLM_",dataN[sz],".RData",sep=""))
	}
	
	glms2 <- list()

	if (j==1) { sT<-Sys.time() }
		bglms2	<-	foreach	(i=1:nsp) %dopar% { tryCatch({ glm(form2, family=binomial(link = "logit"), data=DD_t[[j]][[i]])}, 
							error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
		}

	qbglms2	<-	foreach	(i=1:nsp) %dopar% { tryCatch({ glm(form2, family=quasibinomial(link = "logit"), data=DD_t[[j]][[i]])}, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }

	for (i in 1:length(bglms2)) {
		aucs<-NULL
		tryCatch({ aucs<-c(auc(response=DD_t[[j]][[i]][,1],predict(bglms2[[i]],type="response")),
					auc(response=DD_t[[j]][[i]][,1],predict(qbglms2[[i]],type="response")))}, 
						error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
			if (is.null(aucs)) { 
				aucs<-c(cor(cumsum(predict(bglms2[[i]],type="response")), cumsum(DD_t[[j]][[i]][,1])),
					  cor(cumsum(predict(qbglms2[[i]],type="response")), cumsum(DD_t[[j]][[i]][,1])))
				}			
			if (aucs[1]>=aucs[2]) { glms2[[i]]<-bglms2[[i]] }	
			if (aucs[1]<aucs[2]) { glms2[[i]]<-qbglms2[[i]] }
		}

	save(glms2, file=paste(FD,set_no,"/glm2_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_GLM2_",dataN[sz],".RData",sep=""))
	}
	
	}

##########################################################################################
