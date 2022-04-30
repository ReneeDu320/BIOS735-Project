test_that("ridge logistic is consistent with glmnet when lambda=0", {
  X = matrix(rnorm(1000*3),ncol=3)
  y = rbinom(1000,1,plogis(X[,1]))
  X1 = cbind(1, X)

  fit3 = glm.logit.ridge(X1, y,lambda=0)
  fit4 <- glmnet::glmnet(X, y, family = "binomial", alpha = 0, lambda = 0, standardize = FALSE)
  coef_ridge <- round(as.numeric(fit3$coefficients),3)
  coef_glmnet <- round(as.numeric(c(fit4$a0, as.numeric(fit4$beta))),3)

  #print(coef_ridge)
  #print(coef_glmnet)
  expect_equal(coef_ridge, coef_glmnet)
            
})



test_that("ridge logistic is consistent with glm", {
  X = matrix(rnorm(1000*3),ncol=3)
  y = rbinom(1000,1,plogis(X[,1]))
  X1 = cbind(1, X)
  
  fit3 = glm.logit.ridge(X1, y,lambda=0)
  fit4 <- glm(y ~ X, family = "binomial")
  coef_ridge <- round(as.numeric(fit3$coefficients),5)
  coef_glm <- round(as.numeric(fit4$coefficients),5)
  
  #print(coef_ridge)
  #print(coef_glmnet)
  expect_equal(coef_ridge, coef_glm)
  
})


test_that("simple errors for bad input", {
  
  n <- 100
  n1 <- 40
  p <- 50
  
  X <- matrix(rnorm(n*p),n,p)
  X1 <- matrix(rnorm(n1*p),n1,p)
  
  #set.seed(100)
  #x <- matrix(rnorm(m*n), m, n)
  #f1 <- gl(3,(n/4))
  #f2 <- gl(2, (little.n + 1))
  #f3 <- factor(rep(1:2, times=c(little.n - 4, little.n + 4) ))
  
  expect_error(glm.logit.ridge(cbind(1,X), y))
  expect_error(glm.logit.ridge(cbind(1,X1), y))
  expect_error(glm.logit.ridge(cbind(1,X), y[-1]))
  expect_error(glm.logit.ridge(cbind('a',X), y))
  expect_error(glm.logit.ridge(cbind(1,X), y, lambda=-1))
  
})