##########################################################################################
# UNIVARIATE GENERALIZED LINEAR MODELS WITH LASSO or ELASTICNET PENALTY
##########################################################################################
require(glmnet)

for (j in 1:3) {

	nsp <- length(DD_t[[j]])

	if (j==1) { sT<-Sys.time() }

		glmnet1	<-	foreach	(i=1:nsp) %dopar% { 
		
			tryCatch({ glmnet(y = as.matrix(DD_t[[j]][[i]][,1]), 
							  x = as.matrix(DD_t[[j]][[i]][,-c(1:2,ncol(DD_t[[j]][[i]])-1,ncol(DD_t[[j]][[i]]))]),
							  family = "binomial", 
							  alpha = 1, 
							  nlambda = 200, 
							  standardize = FALSE, 
							  intercept = TRUE)},
						error=function(e){cat("ERROR :",
						conditionMessage(e), "\n")}) 
						
						}
						
	if (j==1) {
		eT<-Sys.time()
		comTimes<-eT-sT
	}

	save(glmnet1, file=file.path(FD,set_no,paste("glmnet1_",j,"_",dataN[sz],".RData",sep="")))
	if (j==1) {
		save(comTimes, file=file.path(FD,set_no,paste("comTimes_GLMNET1_",dataN[sz],".RData",sep="")))
	}
}

##########################################################################################
