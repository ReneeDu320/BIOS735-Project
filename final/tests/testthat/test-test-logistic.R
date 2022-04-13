test_that("ridge logistic correct", {
  X = matrix(rnorm(1000*3),ncol=3)
  y = rbinom(1000,1,plogis(X[,1]))
  X1 = cbind(1, X)

  fit3 = glm.logit.ridge(X1, y)
  fit4 <- glmnet::glmnet(X, y, family = "binomial", alpha = 0, lambda = 1, standardize = FALSE)

  expect_equal(as.numeric(fit3$coefficients),
               c(fit4$a0, as.numeric(fit4$beta)))
})
