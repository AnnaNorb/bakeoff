boralPredict <- function (object, newX = NULL, newrow.ids = NULL, predict.type = "conditional", est = "median", prob = 0.95, lv.mc = 1000, ...) 
{
	require(mvtnorm)
    num.lv <- object$num.lv
    if (!(predict.type %in% c("conditional", "marginal"))) 
        stop("predict.type can only take values conditonal or marginal, for predictions conditonal and marginal on the latent variables respectively")
    if (predict.type == "marginal") {
        message("Marginal predictions take a long time, because there is a lot of (Monte-Carlo) integration involved. Apologies in advance!")
        if (num.lv == 0) {
            message("Please note if there are no latent variables in the model, then marginal and conditional predictions are equivalent.")
            predict.type <- "conditional"
        }
    }
    if (predict.type == "conditional" & !is.null(newrow.ids)) {
        message("For conditional predictions, newrow.ids is ignored since predictions are made conditional on the set of row effects i.e., on the same set of sites.")
        newrow.ids <- NULL
    }
    if (!is.null(newX)) {
        X <- as.matrix(newX)
        if (is.null(object$X.coefs.mean)) 
            stop("Cannot find coefficients for X in object, even though you supplied newX.")
        if (ncol(object$X.coefs.mean) != ncol(newX)) 
            stop("Number of columns in newX does not match number of columns in object$X.coefs.mean.")
        if (predict.type == "conditional") {
            if (object$row.eff != "none") {
                if (nrow(object$row.ids) != nrow(X)) 
                  stop("For conditional predictions, the number of rows in newX must be equal to number of rows in object$row.ids. ")
            }
            if (num.lv > 0) {
                if (nrow(object$lv.mean) != nrow(X)) 
                  stop("For conditional predictions, the number of rows in newX must be equal to number of rows in object$lv.mean. ")
            }
        }
    }
    if (is.null(newX)) 
        X <- object$X
    if (!is.null(newrow.ids)) {
        if (object$row.eff == "none") 
            stop("Cannot find row effects parameters in object, even though you supplied in newrow.ids.")
        newrow.ids <- as.matrix(newrow.ids)
        if (is.null(colnames(newrow.ids))) 
            colnames(newrow.ids) <- paste0("ID", 1:ncol(newrow.ids))
        if (ncol(object$row.ids) != ncol(newrow.ids)) 
            stop("The number of columns in newrow.ids must be equal to number of columns in object$row.ids. ")
        for (k in 1:ncol(newrow.ids)) {
            if (length(unique(object$row.ids[, k])) < length(unique(newrow.ids[, 
                k]))) 
                stop("The number of unique levels is each column of object$row.ids should be greater than or equal than the number of unique levels in the corresponding column of newrow.ids. ")
        }
    }
    if (is.null(newrow.ids)) {
        newrow.ids <- object$row.ids
    }
    if (!is.null(X) & !is.null(newrow.ids)) {
        if (nrow(X) != nrow(newrow.ids)) 
            stop("Number of rows in newX does not match number of rows in newrow.ids")
    }
    if (is.null(object$jags.model)) 
        stop("MCMC samples not found")
    n <- max(nrow(X), nrow(newrow.ids))
    combined.fit.mcmc <- get.mcmcsamples(object)
    all.linpred <- array(NA, dim = c(n, object$p, nrow(combined.fit.mcmc)))
    pt.pred <- lower.linpred <- upper.linpred <- matrix(NA, nrow = n, 
        ncol = object$p)
    if (predict.type == "conditional") {
        for (t in 1:nrow(combined.fit.mcmc)) {
            if (!is.null(X)) 
                cw.X.coefs <- matrix(combined.fit.mcmc[t, grep("X.coefs", 
                  colnames(combined.fit.mcmc))], nrow = object$p)
            if (object$row.eff != "none") {
                cw.row.coefs <- vector("list", ncol(object$row.ids))
                for (k in 1:ncol(object$row.ids)) cw.row.coefs[[k]] <- combined.fit.mcmc[t, 
                  grep(paste0("row.coefs.ID", k), colnames(combined.fit.mcmc))]
            }
            cw.lv.coefs <- matrix(combined.fit.mcmc[t, grep("lv.coefs", 
                colnames(combined.fit.mcmc))], nrow = object$p)
            cw.eta <- matrix(cw.lv.coefs[, 1], nrow = n, ncol = object$p, 
                byrow = TRUE)
            if (num.lv > 0) {
                cw.lv <- matrix(combined.fit.mcmc[t, grep("lvs", 
                  colnames(combined.fit.mcmc))], nrow = n)
                cw.eta <- cw.eta + cw.lv %*% t(cw.lv.coefs[, 
                  2:(num.lv + 1)])
            }
            if (!is.null(X)) 
                cw.eta <- cw.eta + object$X %*% t(cw.X.coefs)
            if (!is.null(object$offset)) 
                cw.eta <- cw.eta + object$offset
            if (object$row.eff != "none") {
                for (k in 1:ncol(object$row.ids)) cw.eta <- cw.eta + 
                  cw.row.coefs[[k]][object$row.ids[, k]]
            }
            all.linpred[, , t] <- cw.eta
        }
    }
    if (predict.type == "marginal") {
        mc.lv <- rmvnorm(n * lv.mc, mean = rep(0, num.lv))
        for (t in 1:nrow(combined.fit.mcmc)) {
            if (t%%100 == 0) 
                message("Onto MCMC sample ", t)
            if (!is.null(X)) 
                cw.X.coefs <- matrix(combined.fit.mcmc[t, grep("X.coefs", 
                  colnames(combined.fit.mcmc))], nrow = object$p)
            if (object$row.eff == "fixed") {
                cw.row.coefs <- vector("list", ncol(newrow.ids))
                for (k in 1:ncol(newrow.ids)) cw.row.coefs[[k]] <- combined.fit.mcmc[t, 
                  grep(paste0("row.coefs.ID", k), colnames(combined.fit.mcmc))]
            }
            if (object$row.eff == "random") {
                cw.row.coefs <- vector("list", ncol(newrow.ids))
                for (k in 1:ncol(newrow.ids)) cw.row.coefs[[k]] <- matrix(rnorm(length(unique(object$row.ids[, 
                  k])) * lv.mc, mean = 0, sd = combined.fit.mcmc[t, 
                  grep(paste0("row.sigma.ID", k), colnames(combined.fit.mcmc))]), 
                  ncol = lv.mc)
            }
            cw.lv.coefs <- matrix(combined.fit.mcmc[t, grep("lv.coefs", 
                colnames(combined.fit.mcmc))], nrow = object$p)
            all.linpred.mc <- array(NA, dim = c(n, object$p, 
                lv.mc))
            if (!is.null(X)) 
                X.eta <- X %*% t(cw.X.coefs)
            for (b in 1:lv.mc) {
                sel.ind <- (n * b - n + 1):(n * b)
                cw.eta <- matrix(cw.lv.coefs[, 1], nrow = n, 
                  ncol = object$p, byrow = TRUE) + mc.lv[sel.ind, 
                  ] %*% t(cw.lv.coefs[, 2:(num.lv + 1)])
                if (!is.null(X)) 
                  cw.eta <- cw.eta + X.eta
                if (!is.null(object$offset)) 
                  cw.eta <- cw.eta + object$offset
                if (object$row.eff == "fixed") {
                  for (k in 1:ncol(newrow.ids)) cw.eta <- cw.eta + 
                    cw.row.coefs[[k]][newrow.ids[, k]]
                }
                if (object$row.eff == "random") {
                  for (k in 1:ncol(newrow.ids)) cw.eta <- cw.eta + 
                    cw.row.coefs[[k]][newrow.ids[, k], b]
                }
                all.linpred.mc[, , b] <- cw.eta
            }
            all.linpred[, , t] <- apply(all.linpred.mc, c(1, 
                2), mean)
            rm(all.linpred.mc)
        }
    }
    for (i in 1:n) {
        for (j in 1:object$p) {
            if (est == "mean") 
                pt.pred[i, j] <- mean(all.linpred[i, j, ])
            if (est == "median") 
                pt.pred[i, j] <- median(all.linpred[i, j, ])
            lower.linpred[i, j] <- quantile(all.linpred[i, j, 
                ], probs = (1 - prob)/2)
            upper.linpred[i, j] <- quantile(all.linpred[i, j, 
                ], probs = 1 - (1 - prob)/2)
        }
    }
    out <- list(linpred = pt.pred, lower = lower.linpred, upper = upper.linpred, all.linpred = all.linpred)
    return(out)
}
