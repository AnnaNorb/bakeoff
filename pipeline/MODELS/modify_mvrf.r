require("MultivariateRandomForest")

build_forest_predict2 <- function (trainX, trainY, n_tree, m_feature, min_leaf, testX) {
    if (class(n_tree) == "character" || n_tree%%1 != 0 || n_tree < 1) 
        stop("Number of trees in the forest can not be fractional or negative integer or string")
    if (class(m_feature) == "character" || m_feature%%1 != 0 || 
        m_feature < 1) 
        stop("Number of randomly selected features considered for a split can not be fractional or negative integer or string")
    if (class(min_leaf) == "character" || min_leaf%%1 != 0 || 
        min_leaf < 1 || min_leaf > nrow(trainX)) 
        stop("Minimum leaf number can not be fractional or negative integer or string or greater than number of samples")
    if (ncol(trainX) != ncol(testX) || nrow(trainX) != nrow(trainY)) 
        stop("Data size is inconsistant")
    theta <- function(trainX) {
        trainX
    }
    results <- bootstrap::bootstrap(1:nrow(trainX), n_tree, theta)

    b = results$thetastar
    Variable_number = ncol(trainY)
    if (Variable_number > 1) {
        Command = 2
    }
    else if (Variable_number == 1) {
        Command = 1
    }
    Y_HAT = matrix(0 * (1:Variable_number * nrow(testX)), ncol = Variable_number, nrow = nrow(testX))
    Y_pred = NULL
    
    MODELS<-list()
    for (i in 1:n_tree) {
        Single_Model = NULL
        X = trainX[b[, i], ]
        Y = matrix(trainY[b[, i], ], ncol = Variable_number)
        Inv_Cov_Y = solve(cov(Y), tol=1e-30)
        
        if (Command == 1) {
            Inv_Cov_Y = matrix(rep(0, 4), ncol = 2)
        }
        
        Single_Model = build_single_tree(X, Y, m_feature, min_leaf, Inv_Cov_Y, Command) 
		MODELS[[i]] = Single_Model
    }
    return(MODELS)
}
environment(build_forest_predict)<-environment(build_single_tree)


mvrf_predict <- function (models, trainY, n_tree, newX) {

	Variable_number = ncol(trainY)
    Y_HAT = matrix(0 * (1:Variable_number * nrow(newX)), ncol = Variable_number, nrow = nrow(newX))

	for (i in 1:n_tree) {
        Y_pred = single_tree_prediction(models[[i]], newX, Variable_number)
        for (j in 1:Variable_number) {
            Y_HAT[, j] = Y_HAT[, j] + Y_pred[, j]
        }
    }
    Y_HAT = Y_HAT/n_tree
	Y_HAT[Y_HAT>1]<-1
	Y_HAT[Y_HAT<0]<-0
    return(Y_HAT)
}
environment(mvrf_predict)<-environment(build_single_tree)


