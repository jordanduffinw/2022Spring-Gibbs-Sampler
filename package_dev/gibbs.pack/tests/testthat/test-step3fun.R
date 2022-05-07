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

test_that("beta_j is a matrix", {
  expect_equal(TRUE, is.matrix(beta_j))
})




####
####
####
sigma2_theta <- 1
beta_j <- step2fun(X, w_j, k_j)[1]
beta_tilde_j <- step2fun(X, w_j, k_j
