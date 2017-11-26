# fitting STOCHASTIC FEED-FORWARD NEURAL NETWORK
##########################################################################################
# library("devtools")
# install_github("davharris/mistnet2")
library('mistnet2')

# INTERPOLATION
##########################################################################################
X <- X_I_train[,2:(ncovar+1)]
Y <- y_I_train

sT<-Sys.time()

mstnt_I <- mistnet(x=X, y=Y,
    n_z = 2,
    layers = list(
        layer(
            activator = sigmoid_activator,
            n_nodes = ncol(X)+2+1,
            weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        ),
        layer(
            activator = sigmoid_activator,
            n_nodes = ncol(X)+2+1,
            weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        ),
        layer(
            activator = sigmoid_activator,
            n_nodes = ncol(Y),
            weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        )
    ),
    error_distribution = make_distribution("BI",bd=1)
)

eT<-Sys.time()
comTimes$MISTN<-eT-sT


##########################################################################################

save(mstnt_I, file=paste(FD,set_no,"/mstnt_I.r",sep=""))

##########################################################################################

# EXTRAPOLATION
##########################################################################################
X <- X_E_train[,2:(ncovar+1)]
Y <- y_E_train

mstnt_E <- mistnet(x=X, y=Y,
    n_z = 2,
    layers = list(
        layer(
            activator = sigmoid_activator,
            n_nodes = ncol(X)+2+1,
            weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        ),
        layer(
            activator = sigmoid_activator,
            n_nodes = ncol(X)+2+1,
            weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        ),
        layer(
            activator = sigmoid_activator,
            n_nodes = ncol(Y),
            weight_prior = make_distribution("NO", mu = 0.0, sigma = 1.0)
        )
    ),
    error_distribution = make_distribution("BI",bd=1)
)

##########################################################################################

save(mstnt_E, file=paste(FD,set_no,"/mstnt_E.r",sep=""))

##########################################################################################
