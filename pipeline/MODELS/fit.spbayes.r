##########################################################################################
# MULTIVARIATE BAYESIAN GENERALIZED LINEAR SPATIAL REGRESSION MODEL
##########################################################################################

require(spBayes)

##########################################################################################

for (j in 1:3) {

Nsamples <- 30000
q <- ncol(y_train[[j]])
A_start <- diag(1,q)[lower.tri(diag(1,q),TRUE)]

Xmv <- mkMvX(lapply(seq_len(ncol(y_train[[j]])), function(X) x_train[[j]]))
Ymv <- matrix(y_train[[j]],ncol=1)

ft <- glm(Ymv~Xmv,family="binomial")
beta_start <- coefficients(ft)
beta_tune <- t(chol(vcov(ft)))

Priors <- list("beta.Flat", "phi.Unif"=list(rep(3/0.75,q), rep(3/0.25,q)),
               "K.IW"=list(q+1, diag(0.1,q)))
Tuning <- list("beta"=beta_tune, "phi"=rep(1,q), "A"=rep(0.1,length(A_start)),"w"=0.5)
Starting <- list("beta"=beta_start[-1], "phi"=rep(3/0.5,q), "A"=A_start, "w"=0)

forms<-list()
for (i in 1:ncol(y_train[[j]])) {
	forms[[i]]<-formula(sprintf("y_train[[j]][,%d]~x_train[[j]][,-1]", i ))
	}	

	if (j==1) { sT<-Sys.time() }

		spBspat <- spMvGLM(forms, family="binomial", coords=s_train[[j]], knots=s_train[[j]], 
						n.samples=Nsamples, cov.model="exponential",starting=Starting, priors=Priors, tuning=Tuning)

		eT<-Sys.time()
		comTimes<-eT-sT
	
	save(spBspat, file=paste(FD,set_no,"/spBspat_",j,"_",dataN[sz],".RData",sep=""))
	if (j==1) {
	save(comTimes, file=paste(FD,set_no,"/comTimes_SPBAYESspat_",dataN[sz],".RData",sep=""))
	}

}
##########################################################################################
