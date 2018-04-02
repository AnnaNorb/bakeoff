
# modified prediction function for GJAM

gjamPredictMod <- function (output, newdata=NULL, y2plot=FALSE, PLOT=FALSE, ylim=NULL) 

{
    xnew <- ydataCond <- interBeta <- NULL
    tiny <- 1e-10
    wHold <- phiHold <- ploHold <- sampleWhold <- NULL
    COND <- F
    ng <- output$modelList$ng
    burnin <- output$modelList$burnin
    nsim <- 500
    if ("nsim" %in% names(newdata)) 
        nsim <- newdata$nsim
    if (is.null(newdata)) {
        if (PLOT) {
            y1 <- output$inputs$y
            y2 <- output$prediction$ypredMu
            if (!is.null(y2plot)) {
                y1 <- y1[, y2plot]
                y2 <- y2[, y2plot]
            }
            tmp <- .bins4data(y1)
            breaks <- tmp$breaks
            bins <- tmp$bins
            nbin <- tmp$nbin
            if (length(bins) > 0) {
                breaks <- bins
                nPerBin <- NULL
            }
            opt <- list(nPerBin = NULL, breaks = breaks, ylimit = ylim, 
                fill = "lightblue", box.col = "darkblue", POINTS = F)
            .plotObsPred(y1, y2, opt = opt)
            abline(0, 1, lwd = 4, col = "white")
            abline(0, 1, lwd = 2, col = "grey", lty = 2)
        }
        return(list(ypredMu = output$modelSummary$ypredMu, ypredSe = output$modelSummary$ypredSd))
    }
    S <- SO <- S1 <- ncol(output$inputs$y)
    Q <- ncol(output$inputs$x)
    n <- nrow(output$inputs$x)
    y <- yp <- output$inputs$y
    x <- output$inputs$x
    xnames <- colnames(x)
    ynames <- colnames(y)
    cindex <- NULL
    notOther <- output$inputs$notOther
    other <- output$inputs$other
    SO <- length(notOther)
    otherpar <- output$modelList$reductList$otherpar
    censor <- output$modelList$censor
    REDUCT <- output$modelList$REDUCT
    notStandard <- output$modelList$notStandard
    NEWX <- F
    if ("xdata" %in% names(newdata)) 
        NEWX <- T
    if ("ydataCond" %in% names(newdata)) 
        COND <- T
    effort <- output$modelList$effort
    effMat <- effort$values
    inSamp <- 1:n
    REDUCT <- output$modelList$REDUCT
    if (REDUCT) {
        otherpar <- output$modelList$reductList$otherpar
        N <- otherpar$N
        r <- otherpar$r
        rndEff <- y * 0
        sigmaerror <- otherpar$sigmaerror
    }
    cuts <- output$parameters$cutMu
    cuts <- cbind(-Inf, 0, cuts, Inf)
    nfact <- output$inputs$factorBeta$nfact
    isFactor <- output$inputs$factorBeta$isFactor
    factorList <- output$inputs$factorBeta$factorList
    contrasts <- output$inputs$factorBeta$contrast
    formula <- output$modelList$formula
    xscale <- output$inputs$standX
    if (is.matrix(xscale)) 
        xscale <- t(xscale)
    facNames <- names(factorList)
    typeNames <- output$modelList$typeNames
    tmp <- .gjamGetTypes(typeNames)
    typeFull <- tmp$typeFull
    typeCols <- tmp$typeCols
    allTypes <- unique(typeCols)
    typeCode <- tmp$TYPES[typeCols]
    FCgroups <- attr(typeNames, "FCgroups")
    CCgroups <- attr(typeNames, "CCgroups")
    CATgroups <- attr(typeNames, "CATgroups")
    condCols <- numeric(0)
    standRows <- output$inputs$standRows
    standMat <- output$inputs$standMat
    standX <- output$inputs$standX
    xmu <- standX[, 1]
    xsd <- standX[, 2]
    intMat <- interBeta$intMat
    notCorCols <- 1:S
    if (NEWX) {
        xnew <- newdata$xdata
        nx <- n <- nrow(xnew)
        colnames(xnew) <- .cleanNames(colnames(xnew))
        effMat <- matrix(1, nx, S)
        holdoutN <- nx
        holdoutIndex <- 1:nx
        if ("effort" %in% names(newdata)) {
            ev <- newdata$effort$values
            effMat <- matrix(1, nx, S)
            effMat[, newdata$effort$columns] <- ev
        }
        effort <- list(columns = c(1:S), values = effMat)
        ydataCond <- NULL
        if (nfact > 0) {
            for (j in 1:nfact) {
                nf <- names(factorList)[j]
                wf <- which(names(xnew) == nf)
                wo <- which(names(output$xnew) == nf)
                wc <- which(names(contrasts) == names(factorList)[j])
                cc <- contrasts[[wc]]
                xnew[[wf]] <- factor(xnew[[wf]], levels = rownames(cc))
                attr(xnew[[wf]], "contrasts") <- cc
            }
        }
        y <- matrix(0, nx, S)
        colnames(y) <- ynames
        yp <- y
        wss <- names(standRows)[names(standRows) %in% names(xnew)]
        xnew[, wss] <- t((t(xnew[, wss]) - standX[wss, "xmean"])/standX[wss, 
            "xsd"])
        tmp <- .gjamXY(formula, xnew, yp, typeNames, notStandard = names(xnew), 
            checkX = F, xscale = xscale)
        x <- tmp$x
        beta <- output$parameters$betaMu
        w <- x %*% beta
        yp <- w * effMat
        wca <- which(typeNames == "CA")
        if (length(wca) > 0) {
            yp[, wca][yp[, wca] < 0] <- 0
        }
        wda <- which(typeNames == "DA")
        if (length(wda) > 0) {
            yp[, wda] <- round(yp[, wda] * effMat[, wda], 0)
            yp[, wda][yp[, wda] < 0] <- 0
        }
        ordCols <- which(typeNames == "OC")
        if (length(ordCols) > 0) {
            tmp <- .gjamGetCuts(yp + 1, ordCols)
            cutLo <- tmp$cutLo
            cutHi <- tmp$cutHi
            for (k in ordCols) {
                yp[, k] <- findInterval(yp[, k], cuts[k, ]) - 
                  1
            }
        }
        if (length(FCgroups) > 0) {
            ntt <- max(FCgroups)
            for (i in 1:ntt) {
                wk <- which(FCgroups == i)
                wo <- which(wk %in% notOther)
                yp[, wk] <- .gjamCompW2Y(yp[, wk, drop = F], 
                  notOther = wo)$ww
            }
        }
        if (length(CCgroups) > 0) {
            print("for CC data total effort (count) is taken as 1000")
            ysum <- rep(1000, n)
            ntt <- max(CCgroups)
            if (ntt > 0) {
                for (i in 1:ntt) {
                  wk <- which(CCgroups == i)
                  wo <- which(wk %in% notOther)
                  yp[, wk] <- .gjamCompW2Y(yp[, wk, drop = F], 
                    notOther = wo)$ww
                  yp[, wk][yp[, wk] < 0] <- 0
                  yp[, wk] <- round(sweep(yp[, wk], 1, ysum, 
                    "*"), 0)
                }
            }
        }
        tmp <- .gjamSetup(typeNames, x, yp, breakList = NULL, 
            holdoutN = NULL, holdoutIndex = NULL, censor = NULL, 
            effort = effort)
        w <- tmp$w
        z <- tmp$z
        yp <- tmp$y
        other <- tmp$other
        plo <- tmp$plo
        phi <- tmp$phi
        ordCols <- tmp$ordCols
        disCols <- tmp$disCols
        compCols <- tmp$compCols
        minOrd <- tmp$minOrd
        maxOrd <- tmp$maxOrd
        censorCA <- tmp$censorCA
        censorDA <- tmp$censorDA
        censorCON <- tmp$censorCON
        ncut <- ncol(cuts)
        corCols <- tmp$corCols
        if (length(corCols) > 0) 
            notCorCols <- notCorCols[-corCols]
        catCols <- which(attr(typeNames, "CATgroups") > 0)
        sampleW <- tmp$sampleW * 0 + 1
        byCol <- byRow <- F
        if (attr(sampleW, "type") == "cols") 
            byCol <- T
        if (attr(sampleW, "type") == "rows") 
            byRow <- T
        indexW <- attr(sampleW, "index")
        inSamp <- 1:n
        byCol <- byRow <- F
        if (attr(sampleW, "type") == "cols") 
            byCol <- T
        if (attr(sampleW, "type") == "rows") 
            byRow <- T
        indexW <- attr(sampleW, "index")
        cdex <- c(1:S)
    }
    if (COND) {
        ydataCond <- newdata$ydataCond
        colnames(ydataCond) <- .cleanNames(colnames(ydataCond))
        condNames <- colnames(ydataCond)
        if ("other" %in% condNames) {
            condNames <- condNames[condNames != "other"]
            ydataCond <- ydataCond[, condNames]
        }
        n <- nrow(x)
        condCols <- match(condNames, colnames(yp))
        yp[, condCols] <- ydataCond
        tmp <- .gjamSetup(typeNames, x, yp, breakList = NULL, 
            holdoutN = NULL, holdoutIndex = NULL, censor = NULL, 
            effort = effort)
        w <- tmp$w
        z <- tmp$z
        yp <- tmp$y
        other <- tmp$other
        plo <- tmp$plo
        phi <- tmp$phi
        ordCols <- tmp$ordCols
        disCols <- tmp$disCols
        compCols <- tmp$compCols
        minOrd <- tmp$minOrd
        maxOrd <- tmp$maxOrd
        censorCA <- tmp$censorCA
        censorDA <- tmp$censorDA
        censorCON <- tmp$censorCON
        ncut <- ncol(cuts)
        corCols <- tmp$corCols
        if (length(corCols) > 0) 
            notCorCols <- notCorCols[-corCols]
        effort <- tmp$effort
        catCols <- which(attr(typeNames, "CATgroups") > 0)
        sampleW <- tmp$sampleW
        sampleW[, -condCols] <- 1
        standRows <- output$inputs$standRows
        standMat <- output$inputs$standMat
        standMu <- output$inputs$standMu
        byCol <- byRow <- F
        if (attr(sampleW, "type") == "cols") 
            byCol <- T
        if (attr(sampleW, "type") == "rows") 
            byRow <- T
        indexW <- attr(sampleW, "index")
        cdex <- c(1:S)[-condCols]
        CCsums <- numeric(0)
        if (!is.null(CCgroups)) {
            ncc <- max(CCgroups)
            for (j in 1:ncc) {
                wjk <- which(CCgroups == j)
                CCsums <- append(CCsums, list(rowSums(y[, wjk])))
            }
        }
    }
    if (length(other) > 0) 
        cdex <- cdex[!cdex %in% other]
    S1 <- length(cdex)
    yg <- yp
    FULL <- F
    if (length(yp) < 10000) 
        FULL <- T
    if (FULL) {
        ygibbs <- wgibbs <- matrix(0, nsim, length(yp))
    }
    pmax <- apply(output$inputs$y/output$modelList$effort$values, 
        2, max)
    ptmp <- 10 * matrix(pmax, n, S, byrow = T)
    ptmp[, ordCols] <- length(ordCols) + 10
    ptmp[, compCols] <- 10
    ptmp[, catCols] <- 10
    if (COND) {
        holdoutN <- 0
        holdoutIndex <- NULL
        ploHold <- phiHold <- NULL
        plo[, -condCols] <- -ptmp[, -condCols]
        phi[, -condCols] <- ptmp[, -condCols]
    }
    else {
        holdoutN <- n
        holdoutIndex <- c(1:n)
        ploHold <- plo
        phiHold <- phi
        plo <- -ptmp
        phi <- ptmp
    }
    .updateW <- .wWrapper(REDUCT, S, effMat, corCols, notCorCols, 
        typeNames, typeFull, typeCols, allTypes, holdoutN, holdoutIndex, 
        censor, censorCA, censorDA, censorCON, notOther, sampleW, 
        byRow, byCol, indexW, ploHold, phiHold, sampleWhold, 
        inSamp)
    ypred <- matrix(0, n, S)
    colnames(ypred) <- ynames
    ypred2 <- wcred <- wcred2 <- ypred
    gvals <- sample(burnin:ng, nsim, replace = T)
    pbar <- txtProgressBar(min = 1, max = nsim, style = 1)
    ig <- 0
    corColC <- cdex[cdex %in% corCols]
    corColW <- which(cdex %in% corCols)
    ddex <- which(notOther %in% cdex)
    cutg <- cuts
    ncut <- ncol(cutg)
    ccols <- which(typeNames != "CON")
    kg <- 1
    rndEff <- 0
    prPresent <- w * 0
    emat <- matrix(0, S, S)
    colnames(emat) <- rownames(emat) <- ynames
    lo <- hi <- lm <- hm <- ess <- emat
    eCont <- output$inputs$factorBeta$eCont
    dCont <- output$inputs$factorBeta$dCont
    lCont <- output$inputs$factorBeta$lCont
    covE <- cov(x %*% dCont)
    frow <- NULL
    if (nfact > 0) {
        frow <- rep(0, Q)
        for (j in 1:nfact) {
            frow[match(factorList[[j]], xnames)] <- j
        }
    }
    q1 <- nrow(eCont)
    fnames <- rownames(eCont)
    facList2 <- factorList
    if (nfact > 0) {
        for (j in 1:nfact) {
            wj <- which(names(xnew) == names(factorList)[j])
            facList2[[j]] <- levels(xnew[[wj]])
        }
    }
    notPA <- which(!typeNames == "PA" & !typeNames == "CON")
    predYs <- NA
    for (g in gvals) {
        bg <- matrix(output$chains$bgibbs[g, ], Q, S)
        muw <- x %*% bg
        if (REDUCT) {
            Z <- matrix(output$chains$sgibbs[g, ], N, r)
            sigmaerror <- output$chains$sigErrGibbs[g]
            K <- output$chains$kgibbs[g, ]
            sg <- .expandSigma(sigmaerror, S, Z = Z, K, REDUCT = T)
        }
        else {
            sg <- .expandSigma(output$chains$sgibbs[g, ], S = S, 
                REDUCT = F)
        }
        alpha <- .sqrtRootMatrix(bg, sg, DIVIDE = T)
        bgg <- bg[, notOther]
        agg <- .sqrtRootMatrix(bgg, sg[notOther, notOther], DIVIDE = T)
        if (nfact > 0) {
            agg <- lCont %*% agg
            for (k in 1:nfact) {
                fk <- factorList[[k]]
                mua <- colMeans(agg[drop = F, fk, ])
                nl <- length(fk)
                agg[fk, ] <- agg[fk, ] - matrix(mua, nl, SO, 
                  byrow = T)
            }
        }
        else {
            agg <- agg[drop = F, -1, ]
        }
        egg <- lCont %*% bgg
        if ("OC" %in% typeCode) {
            cutg[, 3:(ncut - 1)] <- matrix(output$chains$cgibbs[g, 
                ], S)
            tmp <- .gjamGetCuts(yg + 1, ordCols)
            cutLo <- tmp$cutLo
            cutHi <- tmp$cutHi
            plo[, ordCols] <- cutg[cutLo]
            phi[, ordCols] <- cutg[cutHi]
        }
        tmp <- .updateW(rows = 1:n, x, w, yg, bg, sg, alpha, 
            cutg, plo, phi, rndEff = rndEff, sigmaerror, wHold)
        w <- tmp$w
        if (!COND) {
            yg <- tmp$yp
        }
        else {
            tmp <- .conditionalMVN(w, muw, sg, cdex = ddex, S)
            muc <- tmp$mu
            sgp <- tmp$vr
            if (S1 == 1) {
                w[, ddex] <- matrix(rnorm(n, muc, sqrt(sgp[1])))
            }
            else {
                w[, ddex] <- .rMVN(n, muc, sgp)
            }
            muw[, ddex] <- muc
            if (length(corColC) > 0) {
                sgs <- .cov2Cor(sg)
                mus <- x %*% alpha
                muw[, corColC] <- mus[, corColC]
                tmp <- .conditionalMVN(w, mus, sgs, cdex = cdex, 
                  S)
                mus <- tmp$mu
                sgs <- tmp$vr
                muw[, cdex] <- mus
                if (S1 == 1) {
                  w[, ddex] <- matrix(rnorm(n, mus, sqrt(sgs[1])))
                }
                else {
                  w[, ddex] <- .rMVN(n, mus, sgs)
                }
            }
            yg[, -condCols] <- (w * effMat)[, -condCols]
            if (length(ccols) > 0) {
                mmm <- yg[, ccols]
                mmm[mmm < 0] <- 0
                yg[, ccols] <- mmm
            }
            for (k in allTypes) {
                wk <- which(typeCols == k)
                nk <- length(wk)
                wo <- which(wk %in% notOther)
                wu <- which(typeCols[notOther] == k)
                wp <- w[, wk, drop = F]
                groups <- NULL
                if (typeFull[wk[1]] == "countComp") {
                  groups <- CCgroups[wk]
                  nkk <- max(groups)
                  for (j in 1:nkk) {
                    wjk <- which(typeCols[wk] == k & CCgroups[wk] == 
                      j)
                    wno <- which(wk %in% notOther)
                    woo <- which(wk %in% other)
                    www <- w[, wk]
                    www[www < 0] <- 0
                    www <- .gjamCompW2Y(www, notOther = wno)$ww
                    if (COND) {
                      www <- sweep(www, 1, CCsums[[j]], "*")
                    }
                    else {
                      www <- sweep(www, 1, ysum, "*")
                    }
                    yg[, wk] <- www
                  }
                }
                else {
                  if (typeFull[wk[1]] == "fracComp") 
                    groups <- FCgroups[wk]
                  glist <- list(wo = wo, type = typeFull[wk[1]], 
                    yy = yg[, wk, drop = F], wq = wp, yq = yg[, 
                      wk, drop = F], cutg = cutg, censor = censor, 
                    censorCA = censorCA, censorDA = censorDA, 
                    censorCON = censorCON, eff = effMat[, wk, 
                      drop = F], groups = groups, k = k, typeCols = typeCols, 
                    notOther = notOther, wk = wk, sampW = sampleW[, 
                      wk])
                  tmp <- .gjamWLoopTypes(glist)
                  yg[, wk] <- tmp[[2]]
                  yg[, wk] <- .censorValues(censor, yg, yg)[, 
                    wk]
                }
            }
        }
        if (length(ccols) > 0) {
            mmm <- muw[, ccols]
            mmm[mmm < 0] <- 0
            muw[, ccols] <- mmm
        }
        yg[, condCols] <- ydataCond
        yy <- yg
        if ("PA" %in% typeNames) {
            wpa <- which(typeNames == "PA")
            yy[, wpa] <- round(yy[, wpa])
        }
        if (length(notPA) > 0) {
            w0 <- which(yy[, notPA] <= 0)
            w1 <- which(yy[, notPA] > 0)
            yy[, notPA][w0] <- 0
            yy[, notPA][w1] <- 1
        }
        
        #
		predYs <- cbind(predYs,yy)
		#
		
        prPresent <- prPresent + yy
        ig <- ig + 1
        setTxtProgressBar(pbar, ig)
        ypred <- ypred + yg
        ypred2 <- ypred2 + yg^2
        wcred <- wcred + muw
        wcred2 <- wcred2 + muw^2
        ess[notOther, notOther] <- .cov2Cor(t(agg) %*% covE %*% 
            agg)
        emat[notOther, notOther] <- emat[notOther, notOther] + 
            ess[notOther, notOther]
        if (FULL) {
            ygibbs[kg, ] <- as.vector(yg)
            wgibbs[kg, ] <- as.vector(muw)
        }
        kg <- kg + 1
    }
    prPresent <- prPresent/nsim
    ematrix <- emat/nsim
    xunstand <- .getUnstandX(x, standRows, xmu, xsd, intMat)$xu
    yMu <- ypred/nsim
    res <- ypred2/(nsim - 1) - yMu^2
    res[res < tiny] <- tiny
    yPe <- sqrt(res)
    wMu <- wcred/nsim
    res <- wcred2/(nsim - 1) - wMu^2
    res[res < tiny] <- tiny
    wSe <- sqrt(res)
    colnames(yMu) <- colnames(yPe) <- colnames(wMu) <- colnames(wSe) <- ynames
    sdList <- list(yMu = yMu, yPe = yPe, wMu = wMu, wSe = wSe)
    piList <- NULL
    if (FULL) {
        wLo <- matrix(apply(wgibbs, 2, quantile, 0.05), n, S)
        wHi <- matrix(apply(wgibbs, 2, quantile, 0.95), n, S)
        yLo <- matrix(apply(ygibbs, 2, quantile, 0.05), n, S)
        yHi <- matrix(apply(ygibbs, 2, quantile, 0.95), n, S)
        colnames(wLo) <- colnames(wHi) <- colnames(yLo) <- colnames(yHi) <- ynames
        piList <- list(wLo = wLo, wHi = wHi, yLo = yLo, yHi = yHi)
    }
    if (PLOT) {
        oma <- c(0, 0, 0, 0)
        mar <- c(4, 4, 2, 1)
        tcl <- -0.5
        mgp <- c(3, 1, 0)
        par(oma = oma, mar = mar, tcl = tcl, mgp = mgp, bty = "n")
        wy <- which(colnames(y) %in% y2plot & c(1:S) %in% notOther)
        t2plot <- typeNames[wy]
        allTypes <- unique(t2plot)
        mfrow <- .getPlotLayout(length(allTypes) + 1)
        par(mfrow = mfrow, bty = "n", mar = c(1, 2, 3, 1))
        k <- 0
        add <- F
        for (j in 1:length(allTypes)) {
            wk <- which(typeNames == allTypes[j] & c(1:S) %in% 
                notOther)
            ws <- colnames(y)[wk]
            wm <- which(colnames(yMu) %in% colnames(y)[wk])
            wk <- match(colnames(yMu)[wm], colnames(y))
            y1 <- y[, wk]
            if (min(y1) == max(y1)) 
                next
            y2 <- yMu[, wm]
            tmp <- .gjamPlotPars(type = allTypes[j], y1, y2)
            y1 <- tmp$y1
            yp <- tmp$yp
            nbin <- tmp$nbin
            nPerBin <- tmp$nPerBin
            vlines <- tmp$vlines
            xlimit <- tmp$xlimit
            ylimit <- tmp$ylimit
            breaks <- tmp$breaks
            wide <- tmp$wide
            LOG <- tmp$LOG
            POINTS <- F
            MEDIAN <- tmp$MEDIAN
            log <- ""
            if (LOG) 
                log <- "xy"
            if (LOG) {
                wn <- which(y1 > 0 & yp > 0)
                y1 <- y1[wn]
                yp <- yp[wn]
            }
            tmp <- .bins4data(y1, nPerBin = nPerBin, breaks = breaks, 
                LOG = LOG)
            breaks <- tmp$breaks
            bins <- tmp$bins
            nbin <- tmp$nbin
            if (!allTypes[j] %in% c("PA", "CAT")) {
                ncc <- max(c(100, max(y1)/20))
                xy <- .gjamBaselineHist(y1, bins = bins, nclass = ncc)
                xy[2, ] <- ylimit[1] + 0.8 * xy[2, ] * diff(ylimit)/max(xy[2, 
                  ])
                plot(xy[1, ], xy[2, ], col = "tan", type = "s", 
                  lwd = 2, xlim = xlimit, ylim = ylimit, xlab = "Observed", 
                  ylab = "Predicted")
                polygon(xy[1, ], xy[2, ], border = "tan", col = "wheat")
            }
            else {
                y11 <- mean(y1)
                y00 <- 1 - y11
                x11 <- c(-0.07, -0.07, 0.07, 0.07, 0.93, 0.93, 
                  1.07, 1.07, -0.07)
                y11 <- c(0, y00, y00, 0, 0, y11, y11, 0, 0)
                plot(x11, y11, col = "tan", type = "s", lwd = 2, 
                  xlim = xlimit, ylim = ylimit, xlab = "Observed", 
                  ylab = "Predicted")
                polygon(x11, y11, border = "tan", col = "wheat")
            }
            abline(0, 1, lty = 2, lwd = 3, col = "brown")
            abline(h = mean(y1), lty = 2, lwd = 3, col = "tan")
            add <- T
            opt <- list(xlabel = "Observed", ylabel = "Predicted", 
                nbin = nbin, nPerBin = nPerBin, xlimit = xlimit, 
                ylimit = ylimit, breaks = breaks, wide = wide, 
                LOG = LOG, fill = "lightblue", box.col = "darkblue", 
                POINTS = F, MEDIAN = MEDIAN, add = add)
            .plotObsPred(y1, y2, opt = opt)
            if (length(vlines) > 0) 
                abline(v = vlines, lty = 2)
            tt <- allTypes[j]
            if (length(ws) == 1) 
                tt <- paste(ws, tt, sep = "-")
            lab <- paste(letters[j], ") ", tt, sep = "")
            .plotLabel(lab, above = T)
        }
        yp <- colMeans(yMu)
        wy <- match(colnames(yMu), colnames(y))
        opt <- list(xlabel = "Observed", xlimit = NULL, ylimit = NULL, 
            breaks = breaks, wide = wide, LOG = LOG, fill = "lightblue", 
            box.col = "darkblue", POINTS = T, ptcol = "darkblue")
        .plotObsPred(colMeans(y[, wy]), yp, opt = opt)
        abline(0, 1, lty = 2, lwd = 3, col = "brown")
        abline(h = mean(y1), lty = 2, lwd = 3, col = "tan")
        .plotLabel(paste(letters[j + 1], ") By Species", sep = ""), 
            above = T)
    }
    list(x = xunstand, sdList = sdList, piList = piList, prPresent = prPresent, 
        ematrix = ematrix, predYs = predYs[,-1])
}
# <bytecode: 0x7fdf289a0748>
# environment: namespace:gjam
environment(gjamPredictMod) <- as.environment("package:gjam")
environment(gjamPredictMod) <- asNamespace("gjam")

