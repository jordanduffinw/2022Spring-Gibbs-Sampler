test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

#Used wrapperfun_pseudo.R to get all the objects for these tests.
#Obejcts from wrapperfun_psuedo.R are: y, n, j
source("wrapperfun_pseudo.R")

beta_tilde_j <- step2fun(X, w_j, k_j)

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

test_that("beta_j is the correct length", {
  expect_equal(1, length(beta_j))
})

test_that("f_prior_i is the correct length", {
  expect_equal(1, length(f_prior_i))
})

test_that("y_tilde is the correct length", {
  expect_equal(1, length(f_prior_i))
})


####
####
####
#Objects created to perform tests for step3fun
sigma2_theta <- 1
beta_j <- step2fun(X, w_j, k_j)[1]
beta_tilde_j <- step2fun(X, w_j, k_j)
f_prior_i <- f_prior[1]
#I'm not sure how to get alpha here
alpha <- c(1:8)
y_tilde <- k_j/w_j + alpha
