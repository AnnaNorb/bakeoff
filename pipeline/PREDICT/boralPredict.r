boralPredict <- function (object, newX = NULL, newrow.ids = NULL, predict.type = "conditional", est = "median", prob = 0.95, lv.mc = 1000, ...) 
{
    require(mvtnorm)
    num.lv <- object$num.lv
    if (predict.type == "marginal") {
        if(num.lv == 0) {
            message("Please note if there are no latent variables in the model, then marginal and conditional predictions are equivalent.")
            predict.type <- "conditional"
            }
    }

    if (!is.null(newX)) {
        X <- as.matrix(newX)
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
    if (is.null(newrow.ids)) {
        newrow.ids <- object$row.ids
    }

    if (is.null(object$jags.model)) 
        stop("MCMC samples not found")

    n <- max(nrow(X), nrow(newrow.ids))
    combined.fit.mcmc <- get.mcmcsamples(object)
    all.linpred <- array(NA, dim = c(n, object$p, nrow(combined.fit.mcmc)))
    pt.pred <- lower.linpred <- upper.linpred <- matrix(NA, nrow = n, ncol = object$p)

    if (predict.type == "conditional") {
        for (t in 1:nrow(combined.fit.mcmc)) {
            if (!is.null(X)) 
                cw.X.coefs <- matrix(combined.fit.mcmc[t, grep("X.coefs", colnames(combined.fit.mcmc))], nrow = object$p)
            cw.lv.coefs <- matrix(combined.fit.mcmc[t, grep("lv.coefs", colnames(combined.fit.mcmc))], nrow = object$p)
            cw.eta <- matrix(cw.lv.coefs[, 1], nrow = n, ncol = object$p, byrow = TRUE)
            if (num.lv > 0) {
                cw.lv <- matrix(combined.fit.mcmc[t, grep("lvs", colnames(combined.fit.mcmc))], nrow = n)
                cw.eta <- cw.eta + tcrossprod(cw.lv, cw.lv.coefs[, 2:(num.lv + 1)])
            }
            if (!is.null(X)) 
                cw.eta <- cw.eta + tcrossprod(X, cw.X.coefs)
            if (!is.null(object$offset)) 
                cw.eta <- cw.eta + object$offset
            all.linpred[, , t] <- cw.eta
        }
    }

    if (predict.type == "marginal") {
        mc.lv <- rmvnorm(n * lv.mc, mean = rep(0, num.lv))
        for (t in 1:nrow(combined.fit.mcmc)) {
            if (t%%100 == 0) 
                message("Onto MCMC sample ", t)
            if (!is.null(X)) 
                cw.X.coefs <- matrix(combined.fit.mcmc[t, grep("X.coefs", colnames(combined.fit.mcmc))], nrow = object$p)
            cw.lv.coefs <- matrix(combined.fit.mcmc[t, grep("lv.coefs", colnames(combined.fit.mcmc))], nrow = object$p)
            all.linpred.mc <- array(NA, dim = c(n, object$p, lv.mc))
            if (!is.null(X)) 
                X.eta <- tcrossprod(X, cw.X.coefs)

            for (b in 1:lv.mc) {
                sel.ind <- (n * b - n + 1):(n * b)
                cw.eta <- matrix(cw.lv.coefs[, 1], nrow = n, ncol = object$p, byrow = TRUE) + tcrossprod(mc.lv[sel.ind, ], cw.lv.coefs[, 2:(num.lv + 1)])
                if (!is.null(X)) 
                  cw.eta <- cw.eta + X.eta
                all.linpred.mc[, , b] <- cw.eta
            }
            all.linpred[, , t] <- apply(all.linpred.mc, c(1, 2), mean)
            rm(all.linpred.mc)
        }
    }

#     for (i in 1:n) {
#         for (j in 1:object$p) {
#             if (est == "mean") 
#                 pt.pred[i, j] <- mean(all.linpred[i, j, ])
#             if (est == "median") 
#                 pt.pred[i, j] <- median(all.linpred[i, j, ])
#             lower.linpred[i, j] <- quantile(all.linpred[i, j, ], probs = (1 - prob)/2)
#             upper.linpred[i, j] <- quantile(all.linpred[i, j, ], probs = 1 - (1 - prob)/2)
#         }
#     }

    #out <- list(linpred = pt.pred, lower = lower.linpred, upper = upper.linpred, all.linpred = all.linpred)
    return(all.linpred)
}
