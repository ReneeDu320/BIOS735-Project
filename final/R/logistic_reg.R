#' log likelihood function for logistic regression
#' 
#' This function calculates the log likelihood of logistic regression
#' based on given vector of eta and outcome
#' 
#' @param eta an n by 1 vector containing the linear combination of predictors and coefficient
#' @param y an n by 1 vector containing the outcome variable
#' 
#' @return the log likelihood of logistic regression
logLik <- function(eta, y){
  logL = sum(
    dbinom(
      y,
      prob = plogis(eta),
      size = 1,
      log  = TRUE
    )
  )
  return(logL)
}

#' ridge logistic regression using design matrix and Y
#' 
#' this function fit the ridge logistic regression using design matrix X and response variable Y
#' this model use iteratively reweighted least square algorithm to estimate coefficient of X
#' 
#' @param X an n by p matrix containing the design matrix X, the predictors
#' @param y an n by 1 vector containing the outcome variable
#' @param lambda a scalar value as penalty for ridge logistic regression 
#' 
#' @return the IRLS estimate of regression coefficients
#' 
#' @export
glm.logit.ridge <- function(X, y, lambda=1, tol = 1e-7, maxit = 50,trace=FALSE) {
  
  n = dim(X)[1]
  p = dim(X)[2]
  
  # intialize beta values
  intercept  = log(mean(y) / (1 - mean(y)))   # intercept
  beta_t = c(intercept, rep(0, p-1))
  eps = Inf 
  iter = 0
  
  while (eps > tol && iter < maxit) {
    iter = iter + 1
    
    # save previous value
    beta0 = beta_t
    
    # set up values
    
    # eta
    eta_t  = X %*% beta0
    # mu
    mu   = plogis(eta_t)[,1]
    w    = mu * (1 - mu)
    W    = diag(as.vector(w))
    z    = eta_t + (y - mu)/w
    #beta = solve(t(X) %*% W %*% X) %*% (t(X) %*% (W %*% z))
    
    # solve for beta(t+1)
    #beta_t = beta0 + solve(t(X) %*% W %*% X + lambda*diag(p)) %*% (t(X) %*% W %*% z - lambda*beta0)
    beta_t = solve(t(X) %*% W %*% X + 2*lambda*diag(p)) %*% (t(X) %*% W %*% z)
    
    # update the log likelihood
    eta_t1 = X %*% beta_t
    logL = logLik(eta = eta_t1, y = y)
    
    # calculate the euclidean distance, could also use the log likelihood if we wanted
    eps  = sqrt(sum((beta_t - beta0) ^ 2))
    
    
    # print out info to keep track
    if(trace){
      cat(
        sprintf(
          "Iter: %d logL: %.2f eps:%f\n",
          iter,
          logL,
          eps
        )
      )
    }
  }
  
  
  
  list(
    coefficients = beta_t,
    iter = iter,
    tol  = eps,
    loglik  = logL,
    weights = plogis(X %*% beta_t) * (1 - plogis(X %*% beta_t))
  )
}


#' Cross validation for ridge logistic regression using design matrix and Y
#' 
#' this function use cross validation to identify the best lambda for ridge logistic regression
#' 
#' @param X an n by p matrix containing the design matrix X, the predictors
#' @param y an n by 1 vector containing the outcome variable
#' @param lambdas an vector of lambda for the grid search of best lambda
#' @param fold a integer as the number of fold in the cross validation
#' 
#' 
#' @return the best lambda for ridge logistic regression after grid search
#' 
#' @export
cv.glm.logit.ridge <- function(X, y, lambdas, fold=10) {
  #lambdas = lambdas
  CV = c()
  for (j in 1:length(lambdas)){
    
    lambda = lambdas[j]
    #index <- sample(1:fold, nrow(y), replace = T)
    #idx.mat <- createDataPartition(y,times=fold ,p = 0.8, list = FALSE)
    idx.cv.list = createFolds(y, k=fold, list=TRUE)
    resid = 0
    print(paste('lambda=',exp(lambda)))
    for (i in 1: fold){
      print(paste('fold ',i))
      #cat(i)
      idx_all = 1:length(y)
      
      #index_train <- idx.mat[,i]
      #index_test <- idx_all[!idx_all %in% index_train]
      
      index_test <- idx.cv.list[[i]]
      index_train <- idx_all[!idx_all %in% index_test]
      
      X.train <- X[index_train, ]
      y.train <- y[index_train]
      X.test <- X[index_test, ]
      y.test <- y[index_test]
      beta <- glm.logit.ridge(y=y.train, X=X.train, lambda = exp(lambda))$coefficients
      #beta <- glmnet(x=X.train, y=y.train,  alpha=0, lambda = lambda,family='binomial')$beta
      #y.hat = 1/(1+exp(-X.test%*%beta)) > 0.5
      resid <- resid + sum((y.test - 1/(1+exp(-X.test%*%beta)))^2)
      #resid <- resid + sum((y.test - y.hat)^2)
    }
    CV = c(CV, resid/length(y))
  }
  plot(lambdas, CV, type = "b",xlab='log(lambda)')
  lambda = lambdas[which.min(CV)]
  CVscore = min(CV)
  print(paste0("tunning parameter (lambda.min) = ", exp(lambda), "; CV score = ", CVscore))
  return(list(lambda.min=exp(lambda)))
}


#' Predict outcome using fitted model of ridge logistic regression
#' 
#' This function use previously fitted ridge logistic model to predict new outcome using new design matrix 
#' 
#' @param fit previously fitted object using ridge logistic regression
#' @param X an n by p matrix as new design matrix used to predict new outcome
#' @param prob_cut and scalar value between 0 and 1, the probability cutoff to set the cutoff setting outcome as 0 or 1
#' 
#' @return a list object containing the predicted outcome and their correspoding probability
#' 
#' @export
predict.logistic <- function(fit, X, prob_cut = 0.5){
  eta = X %*% fit$coefficients
  y.prob = exp(eta)/(1 + exp(eta))
  y = ifelse(y.prob > prob_cut, 1, 0)
  return(list(y=y, prob=y.prob))
}


