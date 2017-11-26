###############
## Attempts at faster SAMs by implementing an ECM and borrowing strength from the glmnet package (pun intended)
###############
library(MASS); 
library(glmnet)
library(mvabund)

## prev.tol is minimum average prevalence (across spp) in the simulated dataset
create.samlife <- function(family = "binomial", MM, betas, pis, preval.tol = 0.2, spp.intercepts, spp.phis = NUL) {
	MM <- as.matrix(MM); 
	n = nrow(MM);
	s <- length(spp.intercepts); out <- matrix(0, n, s);
	K <- length(pis); 
	group <- spp.preva <- rep(0,s)
	int.adjust <- 0 ## Incrementally bump up the spp intercepts until prevalence.tol is satisfied

	
	while(mean(spp.preva) < preval.tol) {
		int.adjust <- int.adjust + 0.01; 
		groups <- sample(1:K, size = s, prob = pis, replace = T);
		for(j in 1:s) { 
			eta <- cbind(1,MM)%*%c(spp.intercepts[j]+int.adjust,betas[groups[j],]); 
			if(family == "gaussian") out[,j] <- rnorm(n, mean = eta, sd = sqrt(spp.phis[j])) 
			if(family == "binomial") out[,j] <- rbinom(n,1,p = exp(eta)/(1+exp(eta))) 
			if(family == "negative.binomial") out[,j] <- rnbinom(n,mu = exp(eta),size=1/spp.phis[j]) 			
			if(family == "poisson") out[,j] <- rpois(n,lambda= exp(eta)) 			
			}
		spp.preva <- colSums(out != 0)/n 
      }
	
	cat("Intercept adjustment factor:", int.adjust, "\n"); print(groups)
	return(list(resp = out, z = groups, pis = pis, coefs = betas, intercepts = spp.intercepts+int.adjust, preval.tol = preval.tol)) 
	}

	
## Do E-step and calculate the taus	
e.step <- function(X, y, new.betas, new.pis, new.sp.intercepts, new.sp.phis, family, get.fitted = FALSE) {
	s <- ncol(y); n <- nrow(y); K <- length(new.pis)
	out.taus <- matrix(0,s,K)
	sp.logl <- rep(0,s) ## Species specific incomplete logL
	if(get.fitted) fitted.values <- array(0,dim=c(K,n,s))

	
	for(k in 1:K) {
		new.etas <- X%*%new.betas[k,]

		for(j in 1:s) {
			new.etas2 <- new.sp.intercepts[j] + new.etas
			if(family == "gaussian") { 
				if(get.fitted) fitted.values[k,,j] <- new.etas2
				out.taus[j,k] <- (sum(dnorm(y[,j], mean = new.etas2, sd = sqrt(new.sp.phis[j]), log = TRUE))) 
				}
			if(family == "binomial") {
				check.p <- exp(new.etas2)/(1+exp(new.etas2)); 
				check.p[check.p < 1e-4] <- 1e-4; check.p[check.p > (1-1e-4)] <- (1-1e-4); 
				if(get.fitted) fitted.values[k,,j] <- check.p
				out.taus[j,k] <- (sum(dbinom(y[,j], 1, p = check.p, log = TRUE))) 
				}
			if(family == "poisson") {
				if(get.fitted) fitted.values[k,,j] <- exp(new.etas2)
				out.taus[j,k] <- (sum(dpois(y[,j], lambda = exp(new.etas2), log = TRUE))) 
				}
			if(family == "negative.binomial") { 
				if(get.fitted) fitted.values[k,,j] <- exp(new.etas2)
				out.taus[j,k] <- (sum(dnbinom(y[,j], mu = exp(new.etas2), size = 1/new.sp.phis[j], log = TRUE))) 
				}
		} }

	for(j in 1:s) {
		eps <- max(out.taus[j,])
		sp.logl[j] <- log(sum(new.pis*exp(out.taus[j,]-eps))) + eps 
		}
	for(j in 1:s) { for(k in 1:K) {
		out.taus[j,k] <- exp((log(new.pis[k]) + out.taus[j,k]) - sp.logl[j]) 
		} }
				
	full.logl <- sum(sp.logl)
	out.list <- list(taus = out.taus, sp.logl = sp.logl, logl = full.logl)
	if(get.fitted) out.list$fitted = fitted.values
	return(out.list) 
	}
     
     
     
## Generate starting values using K-means clustering
get.startvals <- function(X, y, K, family, tol = 0.1) {
	n <- nrow(y); 
	s <- ncol(y)
	prev.min <- floor(n*tol); 
	sel.omit.spp <- which(colSums(y>0) <= prev.min) ## Select spp to omit

	starting.sam <- list(sp.intercepts = rep(0,s), sp.phis = rep(1,s));
	cat("Fitting separate GLMs...\n")
	fit1 <- manyglm(mvabund(y) ~ X, family = family)
	starting.sam$sp.intercepts <- fit1$coefficients[1,]

	if(family == "negative.binomial") { starting.sam$sp.phis <- fit1$phi }

	cat("Clustering...\n");  
	fmmvnorm <- kmeans(x = t(fit1$coefficients[-1,-sel.omit.spp]), centers = K, iter.max = 100, nstart = 50); 
	starting.sam$betas <- fmmvnorm$centers;
	rownames(starting.sam$betas) <- 1:K; 
	
	taus <- matrix(0,ncol(y), K)
	for(j in 1:length((1:s)[-sel.omit.spp])) taus[(1:s)[-sel.omit.spp][j],fmmvnorm$cluster[j]] <- 1
	taus[sel.omit.spp,] <- matrix(runif(length(sel.omit.spp)*K),length(sel.omit.spp), K)
	taus <- taus/rowSums(taus)
	magical.alpha <- (1-0.8*K)/(0.8*(2-K)-1) ## Dunstan et al., 2013, JABES
	starting.sam$taus <- (2*magical.alpha*taus-magical.alpha+1)/(2*magical.alpha - magical.alpha*K + K)
	starting.sam$pis <- colMeans(starting.sam$taus)
	
	return(starting.sam) 
	}

	
lseq <- function (from, to, length, decreasing = F) {
	stopifnot(from > 0)
	out <- 10^(seq(log10(from), log10(to), length.out = length))
	out <- out[order(out, decreasing = decreasing)]; 
	return(out)
	}

# ## Calculate predicted for all archetypes
# ## mean.spp.intercept = T uses intercept to be the mean of species-specific intercepts of species classified into that archetype
# ## mean.spp.intercept = F uses intercept to be the mean of all species-specific intercepts 
# ## arb.spp.inter allows custom intercept to be inserted
# predict.psams <- function(psams, newX, dist = "binomial", arb.sp.intercept = NULL, mean.sp.intercept = T) {
# 	if(ncol(newX) != ncol(psams$betas)) stop("Number of coefficients does not match up in psams and newX!")
# 	if(is.null(arb.sp.intercept) & mean.sp.intercept == FALSE) arb.spp.inter2 <- rep(mean(psams$sp.intercept), length(psams$pi))
# 	if(is.null(arb.sp.intercept) & mean.sp.intercept == TRUE) { 
# 		best.arch <- rep(NA, nrow(psams$taus)) 
# 		for(j in 1:nrow(psams$taus)) best.arch[j] <- which(psams$taus[j,] == max(psams$taus[j,]))
# 		arb.spp.inter2 <- as.vector(by(psams$sp.intercept, best.arch, mean)) }
# 	if(!is.null(arb.sp.intercept)) arb.spp.inter2 <- rep(arb.sp.intercept, length(psams$pi))
# 		
# 	K <- length(psams$pi); 
# 	predict.etas <- predict.mus <- matrix(NA, nrow(newX), length(psams$pi))
# 	for(k in 1:K) { predict.etas[,k] <- arb.spp.inter2[k] + as.matrix(newX)%*%as.vector(psams$betas[k,]) }
# 	if(dist == "binomial") predict.mus <- exp(predict.etas)/(1+exp(predict.etas))	
# 	if(dist == "negative.binomial") predict.mus <- exp(predict.etas)
# 	dimnames(predict.mus) <- dimnames(predict.etas) <- list(n = 1:nrow(newX), K = names(psams$pi)) 
# 	return(list(predict.etas = round(predict.etas, 5), predict.mus = round(predict.mus,5))) }

## Calculate predictions for individual spp, as weighted averages over archetypes
## mean.spp.intercept = T uses intercept to be the mean of species-specific intercepts of species classified into that archetype
## mean.spp.intercept = F uses intercept to be the mean of all species-specific intercepts 
## arb.spp.inter allows custom intercept to be inserted
predict.sams <- function(psams, newX, family = "binomial") {
	if(ncol(newX) != ncol(psams$betas)) stop("Number of coefficients does not match up in psams and newX!")
		
	K <- length(psams$pi); 
	s <- length(psams$sp.intercepts)
	n <- nrow(newX)

	## To calculate fitted values, take a linear combination of the fitted mus from each component
	predict.mus <- predict.y <- predict.etas <- matrix(0,nrow(newX),s)
 	for(k in 1:K) { 
 		etas.K <- matrix(psams$sp.intercepts,nrow(newX),s,byrow=TRUE) + matrix(as.matrix(newX)%*%psams$betas[k,],nrow(newX),s,byrow=FALSE)
 		if(family == "gaussian") mus.K <- etas.K	
 		if(family == "binomial") mus.K <- exp(etas.K)/(1+exp(etas.K))	
 		if(family %in% c("poisson","negative.binomial")) mus.K <- exp(etas.K)
 		predict.mus <- predict.mus + matrix(psams$taus[,k],nrow(newX),s,byrow=T)*mus.K
 		}

# 	for(k in 1:K) { 
# 		etas.K <- matrix(psams$sp.intercepts,nrow(newX),s,byrow=TRUE) + matrix(as.matrix(newX)%*%psams$betas[k,],nrow(newX),s,byrow=FALSE)
# 		predict.etas <- predict.etas + matrix(psams$taus[,k],nrow(newX),s,byrow=T)*etas.K
# 		}
# 	if(family == "binomial") predict.mus <- exp(predict.etas)/(1+exp(predict.etas))	
# 	if(family %in% c("poisson","negative.binomial")) predict.mus <- exp(predict.etas)

		
	## To generate an actual response matrix; simulate component-label then simulated response conditional
	for(j in 1:s) {
		get.z <- sample(1:K, 1, prob = psams$taus[j,])
		etas.K <- psams$sp.intercepts[j] + as.matrix(newX)%*%psams$betas[k,]
		if(family == "binomial") predict.y[,j] <- rbinom(n, 1, p=exp(etas.K)/(1+exp(etas.K))	)
		if(family == "poisson") predict.y[,j] <- rpois(n, lambda = exp(etas.K))
		if(family == "negative.binomial") predict.y[,j] <- rnbinom(n, mu = exp(etas.K), size = 1/psams$sp.phis[j])		
		}
		
	rownames(predict.mus) <- rownames(predict.y) <- 1:nrow(newX); 
	colnames(predict.mus) <- colnames(predict.y) <- names(psams$sp.intercepts)

	return(list(predict.mus = round(predict.mus,5), predict.y = predict.y)) 
	}

	
	
## Wrapper for doing ECM
#y = Y; family = "binomial"; restarts = 5; K = 3; max.steps = 100; starting.sam = NULL; trace = TRUE; lambda = 0; nlambda = 100; pen.type = "ADL"; pen.weights = NULL
#X = big.MM; y = as.matrix(resp2); family = "binomial"; pen.weights = 1/abs(upsams$betas); lambda = 0; restarts = 1; max.steps = 100; starting.sam = NULL; pen.type = "ADL"
#X = MM; y = as.matrix(test.y$resp); family = candidate.family; restarts = 3; trace = TRUE; pen.weights = NULL; lambda = 0; restarts = 1; max.steps = 100; starting.sam = NULL; pen.type = "ADL"; nlambda = 100


psams.coord <- function(X, y, family, pen.weights = NULL, lambda = 0, restarts = 5, nlambda = 100, K, max.steps = 100, starting.sam = NULL, pen.type = "ADL", trace = TRUE) {
	if(!is.matrix(y)) stop("y must a matrix.")
	if(!(family %in% c("gaussian","poisson","negative.binomial","binomial"))) stop("Family type currently not allowed...sorry")

	if(!(pen.type %in% c("ADL","MIXGL1","MIXGL2"))) stop("Penalty type currently not allowed...sorry")
	if(is.null(pen.weights)) pen.weights <- matrix(1,nrow=K,ncol=ncol(X))
	if(!is.null(pen.weights)) { 
		if(!is.matrix(pen.weights)) stop("If specified, pen.weights should be a K-by-ncol(X) matrix")
		pen.weights[pen.weights > 1e6] <- 1e6 
		}
	
	n <- nrow(y); s <- ncol(y)
	if(family %in% c("negative.binomial","poisson")) glmnet.family <- "poisson"
	if(family %in% c("gaussian","binomial")) glmnet.family <- family
	
	for(t in 1:restarts) {
		message()
		
		if(is.null(starting.sam)) {
			taus <- matrix(runif(s*K),s); taus <- taus/rowSums(taus)
			cw.betas <- matrix(runif(K*ncol(X)),K,ncol(X))
			cw.sp.intercepts <- rep(0,s); cw.sp.phis <- rep(0.05,s)
			cw.pis <- colSums(taus)/s 
			}

		if(!is.null(starting.sam)) {
			taus <- starting.sam$taus 
			magical.alpha <- (1-0.8*K)/(0.8*(2-K)-1) ## Dunstan et al., 2013, JABES
			taus <- (2*magical.alpha*taus-magical.alpha+1)/(2*magical.alpha - magical.alpha*K + K)
			cw.betas <- starting.sam$betas
			cw.sp.intercepts <- starting.sam$sp.intercepts
			cw.sp.phis <- starting.sam$sp.phis
			cw.pis <- colSums(taus)/s 
			}
			
		diff.logl <- -100; cw.logl <- -Inf; new.logl <- 10
		mod <- list(logl = -Inf) 
		
		counter <- 1
		while((diff.logl < 0.999 | diff.logl > 1.001) & counter <= max.steps) {
			#if(diff.logl > 1) break;
		
			## If any pi hits 0, restart with new random starts
			if(any(cw.pis < 0.005)) { 
				cat("pi has gone to zero - restarting fitting\n")
				taus <- matrix(runif(s*K),s); taus <- taus/rowSums(taus); 
				cw.pis <- colSums(taus)/s; 
				cw.betas <- matrix(runif(K*ncol(X)),K,ncol(X))
				cw.sp.intercepts <- rep(0,s)
				cw.sp.phis <- rep(0.05,s)
				counter <- 1; 
				}

				
			## CM-step, using LQA to approximate all penalties
			new.pis <- colMeans(taus)
			new.sp.intercepts <- cw.sp.intercepts
			new.sp.phis <- cw.sp.phis

			## Update spp-specific intercepts
			llogl.intercept <- function(x, j) {
				out <- 0
				for(k in 1:K) { 
					cw.eta <- x + X%*%cw.betas[k,]
					if(family == "gaussian") 
						out <- out + sum(taus[j,k]*dnorm(y[,j], mean = cw.eta, sd = sqrt(cw.sp.phis[j]), log = TRUE))
					if(family == "binomial") 
						out <- out + sum(taus[j,k]*dbinom(y[,j], size = 1, p = binomial()$linkinv(cw.eta), log = TRUE))
					if(family == "poisson") 
						out <- out + sum(taus[j,k]*dpois(y[,j], lambda = poisson()$linkinv(cw.eta), log = TRUE))
					if(family == "negative.binomial") 
						out <- out + sum(taus[j,k]*dnbinom(y[,j], mu = exp(cw.eta), size = 1/cw.sp.phis[j], log = TRUE))
					}
				return(out)	
				}
			
			llogl.phi <- function(x, j, family) {
				out <- 0
				for(k in 1:K) { 
					cw.eta <- cw.sp.intercepts[j] + X%*%cw.betas[k,]
					if(family == "negative.binomial") 
						cw.out <- sum(taus[j,k]*dnbinom(y[,j], mu = exp(cw.eta), size = 1/x, log = TRUE))
					if(family == "gaussian") 
						cw.out <- sum(taus[j,k]*dnorm(y[,j], mean = cw.eta, sd = sqrt(x), log = TRUE))					
					out <- out + cw.out
					}
				return(out)	
				}
				
			for(j in 1:s) {
				update.int <- optimize(f = llogl.intercept, interval = c(-30,30), maximum = TRUE, j = j)
				new.sp.intercepts[j] <- update.int$maximum
				new.sp.phis[j] <- 1
				
				if(family %in% c("gaussian","negative.binomial")) {
					update.int <- optimize(f = llogl.phi, interval = c(0.001,100), maximum = TRUE, j = j, family = family)
					new.sp.phis[j] <- update.int$maximum
					}
				}
				
				
			## Update archetype-specific coefficients				
			new.betas <- cw.betas
			if(pen.type == "ADL") actual.pen.weights <- pen.weights
			if(pen.type == "MIXGL1") { 
				l1norm <- sqrt(apply(pen.weights*abs(new.betas),2,sum)); actual.pen.weights <- 0.5*pen.weights/matrix(l1norm,K,ncol(X),byrow=T) 
				}
			if(pen.type == "MIXGL2") { 
				l2norm <- sqrt(apply(new.betas^2,2,sum)); actual.pen.weights <- pen.weights*abs(new.betas)/matrix(l2norm,K,ncol(X),byrow=T) 
				}
			actual.pen.weights[actual.pen.weights > 1e6] <- 1e6
			if(family == "negative.binomial") 
				get.mus <- e.step(X, y, cw.betas, new.pis, new.sp.intercepts, new.sp.phis, family, get.fitted = TRUE)$fitted

				
			for(k in 1:K) {
				obs.weights <- rep(taus[,k],each=n)
				if(family == "negative.binomial") 
					obs.weights <- rep(taus[,k],each=n)/(1+rep(new.sp.phis,each=n)*as.vector(get.mus[k,,]))

				if(lambda == 0) { 
# 					llogl.betas <- function(params, k) {
# 						out <- 0
# 						for(j in 1:s) { 
# 							cw.eta <- new.sp.intercepts[j] + X%*%params
# 							if(family == "binomial") 
# 								out <- out + sum(taus[j,k]*dbinom(y[,j], size = 1, p = binomial()$linkinv(cw.eta), log = TRUE))
# 							if(family == "poisson") 
# 								out <- out + sum(taus[j,k]*dpois(y[,j], lambda = poisson()$linkinv(cw.eta), log = TRUE))
# 							if(family == "negative.binomial") 
# 								out <- out + sum(taus[j,k]*dnbinom(y[,j], mu = exp(cw.eta), size = 1/cw.sp.phis[j], log = TRUE))
# 							}
# 						return(out)	
# 						}
# 					fit1 <- optim(par = cw.betas[k,], fn = llogl.betas, k = k, method = "BFGS", control = list(fnscale = -1, trace = 0))
#  					new.betas[k,] <- fit1$par
				
 					fit1 <- glmnet(x = X[rep(1:nrow(X),s),], y = as.vector(unlist(y)), family = glmnet.family, weights = obs.weights+1e-6, offset = new.sp.intercepts[rep(1:length(new.sp.intercepts),each=n)], nlambda = nlambda, intercept = FALSE) 
 					#fit1 <- glm.fit(x = X[rep(1:nrow(X),s),], y = as.vector(unlist(y)), family = binomial(), weights = obs.weights+1e-6, offset = new.sp.intercepts[rep(1:length(new.sp.intercepts),each=n)], intercept = FALSE) 
 					new.betas[k,] <- coef(fit1)[,ncol(coef(fit1))][-1]
					}
				if(lambda > 0) {
					fit1 <- glmnet(x = X[rep(1:nrow(X),s),], y = as.vector(unlist(y)), family = glmnet.family, weights = obs.weights, offset = new.sp.intercepts[rep(1:length(new.sp.intercepts),each=n)], nlambda = nlambda, intercept = FALSE, penalty.factor = actual.pen.weights[k,]) 
					new.betas[k,] <- coef(fit1, s = lambda)[-1] 
					}
				}

				
			## E-step
			do.estep <- try(e.step(X, y, new.betas, new.pis, new.sp.intercepts, new.sp.phis, family),silent=T)
			if(!is.finite(do.estep$logl)) { mod <- list(logl = -Inf); break; }
			if(is.finite(do.estep$logl)) {
				taus <- do.estep$taus
				new.logl <- do.estep$logl
				diff.logl <- abs(new.logl/cw.logl)
			
				#print(round(new.pis,4))
				print(round(new.betas,4))
				if(trace) cat("Iteration: ", counter, "| New loglik", round(new.logl,3),  "| Ratio loglik", diff.logl, "\n"); 
				
				cw.pis <- new.pis; cw.betas <- new.betas
				cw.sp.intercepts <- new.sp.intercepts; cw.sp.phis <- new.sp.phis
				cw.logl <- new.logl
				counter <- counter + 1 
				}	
			}
		
		
		if(new.logl > mod$logl) {
			mod <- list(logl = new.logl, betas = new.betas, sp.intercepts = new.sp.intercepts, sp.phis = new.sp.phis, pis = new.pis, taus = taus, family = family) 
			names(mod$pis) <- paste("Archetype", 1:K, sep = ""); 
			mod$taus <- as.data.frame(round(mod$taus,5)); colnames(mod$taus) <- names(mod$pis); rownames(mod$taus) <- paste("Sp",1:s,sep="")			
			mod$entropy <- -sum(as.vector(mod$taus[mod$taus != 0])*log(unlist(mod$taus[mod$taus != 0]))); 
			mod$betas <- round(mod$betas,6); rownames(mod$betas) <- names(mod$pis); 
			names(mod$sp.intercepts) <- rownames(mod$taus)
			names(mod$sp.phis) <- rownames(mod$phis)
			
			mod$effect.param <- sum(abs(mod$betas) > 1e-5) + K + s + ifelse(family == "negative.binomial",s,0)
			mod$ics <- -2*mod$logl + c(2,log(s),log(s))*mod$effect.param + c(0,0,2*mod$entropy)
			names(mod$ics) <- c("AIC","BIC with log(# of species)", "ICL")
			}
		
		
		}
	
	return(mod) }	

	
	
## Soft thresholding function		
soft.thresh <- function(a,b) {
     if(a > 0 & b < abs(a)) out <- a - b
     if(a < 0 & b < abs(a)) out <- a + b
     if(b >= abs(a)) out <- 0
     return(out) }

				
####################
# load("testdat.RData")
# X = datnb$enviro; y = datnb$resp; 
# family = "negative.binomial"
# K <- 3
# start.time <- proc.time()
# #startsam <- get.startvals(X, y, K, family)
# upsams <- psams.coord(X, y, family, pen.weights = matrix(1,K,ncol(X)), lambda = 0, restarts = 2, K = K, max.steps = 100)
# # #test.mod <- psams.coord(X, y, family, pen.weights = matrix(1,K,ncol(X)), lambda = 0, restarts = 2, K = K, max.steps = 100, starting.sam = startsam)
# end.time <- proc.time()

# lambda.seq <- seq(1e-3,10,length=20) 
# all.betas <- array(0,dim=c(length(lambda.seq),K,ncol(X)))
# test.mod <- psams.coord(X, y, family, pen.weights = 1/abs(upsams$betas), lambda = lambda.seq[1], restarts = 1, K = K, max.steps = 100, starting.sam = upsams, pen.type = "adl")
# all.betas[1,,] <- test.mod$betas
# # # 
# for(k in 2:length(lambda.seq)) {
# 	next.fit <- psams.coord(X, y, family, pen.weights = 1/abs(upsams$betas), lambda = lambda.seq[k], restarts = 1, K = K, max.steps = 100, 
# 		starting.sam = test.mod, pen.type = "adl")
# 	print(round(next.fit$betas,4))
# 	all.betas[k,,] <- next.fit$betas
# 	test.mod <- next.fit }

#test.mod <- psams.coord(X, y, family, pen.weights = 1/abs(upsams$betas), lambda = 100, restarts = 1, K = K, max.steps = 100, starting.sam = upsams, pen.type = "mixgl1")

# load("testglm.RData")
# X = datb$X; y = as.matrix(datb$resp); 
# family = "binomial"
# K <- 1
# test.mod <- psams.coord(X, y, family, pen.weights = matrix(1,K,ncol(X)), lambda = 1e-10, restarts = 1, K = K, max.steps = 100)
# fitb <- glm(y ~ X, family = "binomial")
# 
# load("testglm.RData")
# X = datnb$X; y = as.matrix(datnb$resp); 
# family = "negative.binomial"
# K <- 1
# test.mod <- psams.coord(X, y, family, pen.weights = matrix(1,K,ncol(X)), lambda = 1e-10, restarts = 1, K = K, max.steps = 100)
# library(MASS)
# fitnb <- glm.nb(y ~ X)


