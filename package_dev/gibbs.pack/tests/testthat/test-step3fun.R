test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

#Used wrapperfun_pseudo.R to get all the objects for these tests.
#Obejcts from wrapperfun_psuedo.R are: y, n, j
source("wrapperfun_pseudo.R")

#Create the output object for step2fun
beta_tilde_j <- step2fun(X, w_j, k_j)


#create step3fun input object sigma2_theta
sigma2_theta <- 1

test_that("sigma2_theta is the correct length", {
  expect_equal(1, length(sigma2_theta))
})

test_that("sigma2_theta is the correct value", {
  expect_equal(1, sigma2_theta)
})

test_that("sigma2_theta is the correct length", {
  expect_equal(1, length(sigma2_theta))
})

test_that("sigma2_theta is the correct length", {
  expect_equal(1, length(sigma2_theta))
})

#Create step3fun input object beta_j
beta_j <- as.matrix(beta_tilde_test$beta)

test_that("beta_j is the correct length", {
  expect_equal(j, length(beta_j))
})

test_that("beta_j is a matrix", {
  expect_equal(TRUE, is.matrix(beta_j))
})


#Create step3fun input object f_prior_i
f_prior_i <- f_prior[1]

test_that("f_prior_i is the correct length", {
  expect_equal(1, length(f_prior_i))
})

#Create object y_tilde, a function taken from GibbsSampler-method.R, line 75
  #Changed omega to omega_mat and beta_tilde to beta_tilde_test
y_tilde <- do.call(cbind, lapply(1:4,function(j){(kappa * (1/omega_mat))[,j] + beta_tilde_test$alpha[j]}))

test_that("y_tilde has the correct number of rows", {
  expect_equal(nrow(y), nrow(y_tilde))
})

test_that("y_tilde has the correct number of columns", {
  expect_equal(ncol(y), ncol(y_tilde))
})

test_that("y_tilde is a matrix", {
  expect_equal(TRUE, is.matrix(y_tilde))
})

#Create step3fun input object y_tilde_i
y_tilde_i <- y_tilde[1,]

test_that("y_tilde_i is the correct length", {
  expect_equal(j, length(y_tilde_i))
})

test_that("y_tilde_i is the correct values", {
  expect_equal(y_tilde_i, y_tilde[1,])
})

test_that("y_tilde_i is a vector", {
  expect_equal(TRUE, is.vector(y_tilde_i))
})

#Create step3fun output object theta_i
theta_i <- step3fun(beta_j, f_prior_i, y_tilde_i, sigma2_theta=1)

test_that("theta_i is the correct length", {
  expect_equal(1, length(theta_i))
})

test_that("theta_i is a vector", {
  expect_equal(TRUE, is.vector(theta_i))
})
