##########################################################################################
# BASIC UNIVARIATE GENERALIZED LINEAR MODELS PREDICTIONS (with parameter uncertainty)
##########################################################################################

# function for incorporating parameter uncertainty
##########################################################################################
require(mvtnorm)
predict.glm.sdf <- function( glm.mod, predX, paramUncert=TRUE, REPs=100, doPlot=FALSE) {
if (paramUncert==TRUE){
	vcovy <- vcov( glm.mod) #will this always be available?  Will it be Pos Def?
    if( !is.element( "sp", colnames( predX)))
    	predX$sp <- rep( -999, nrow( predX))  #-999 is a dummy placeholder.  Could change na.action too...
    	X_mod_pred <- model.matrix( glm.mod$formula, data=predX)
    	ranParams <- t( rmvnorm( REPs, coef( glm.mod), sigma=vcovy))
    	ranLinPreds <- X_mod_pred %*% ranParams
    	ranPreds <- glm.mod$family$linkinv( ranLinPreds) #this code will remain constant even if you change the link.
  	} else {
  		ranPreds <- predict( glm.mod, type='response', newdata=predX)
  		ranPreds <- matrix( rep( ranPreds, times=REPs), ncol=REPs)  #OK, the name of the variable is not descriptive here.  It is not random.
  	}
  
  ranY <- matrix( rbinom( n=length( ranPreds), size=1, prob=ranPreds), nrow=nrow( ranPreds), ncol=ncol( ranPreds))
  	# attributes(ranY)$ranPreds <- ranPreds
	if (doPlot){
		matplot( predict( glm.mod, type="response", newdata=predX), ranPreds, pch='.', cex=1.5)
    	abline(0,1, lwd=2)
    }
  return( ranY)
}
##########################################################################################

for (j in 1:3) {

	load(file=paste(FD2,set_no,"/glm1_",j,"_",dataN[sz],".RData",sep=""))

	nsp <- length(DD_v[[j]])
	nsites <- nrow(DD_v[[j]][[1]])

	glm1_PAs <- foreach(i=1:nsp)  %dopar% { tryCatch({ predict.glm.sdf(glms1[[i]], predX=DD_v[[j]][[i]], paramUncert=FALSE) },
	                                                    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	glm1_PAs <- aperm(simplify2array( glm1_PAs), c(1,3,2))

	save(glm1_PAs, file=paste(PD2,set_no,"/glm1_PAs_",j,"_",dataN[sz],".RData",sep=""))

	glm1b_PAs <- foreach(i=1:nsp)  %dopar% { tryCatch({ predict.glm.sdf(glms1[[i]], predX=DD_v[[j]][[i]], paramUncert=TRUE) },
	                                                    error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) }
	glm1b_PAs <- aperm(simplify2array( glm1b_PAs), c(1,3,2))

	save(glm1b_PAs, file=paste(PD2,set_no,"/glm1b_PAs_",j,"_",dataN[sz],".RData",sep=""))

	rm(glms1)
	rm(glm1b_PAs)
	rm(glm1_PAs)
	gc()

}

##########################################################################################
