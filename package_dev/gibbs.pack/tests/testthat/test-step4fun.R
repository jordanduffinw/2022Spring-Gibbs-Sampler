test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


#Used wrapperfun_pseudo.R to get all the objects for these tests.
#Obejcts from wrapperfun_psuedo.R are: y, n, j
source("wrapperfun_pseudo.R")

theta_i <- step3fun(beta_j, f_prior_i, y_tilde_i)

test_that("K_rho is of correct type", {
  expect_equal(TRUE, is.matrix(K_rho))
})

test_that("K_rho has the correct number of rows", {
  expect_equal(nrow(n), nrow(K_rho))
})

test_that("K_rho has the correct number of columns", {
  expect_equal(nrow(n), ncol(K_rho))
})

test_that("theta is of correct type", {
  expect_equal(TRUE, is.matrix(theta))
})

test_that("theta has the correct number of rows", {
  expect_equal(nrow(n), nrow(theta))
})

test_that("theta has the correct number of columns", {
  expect_equal(2, ncol(theta))
})

test_that("f_t is of correct type", {
  expect_equal(TRUE, is.vector(f_t))
})

test_that("f_t has the correct number of rows", {
  expect_equal(1, nrow(f_t))
})

test_that("f_t has the correct number of columns", {
  expect_equal(1, ncol(f_t))
})



####
####
####
#Objects created to perform tests for step4fun
K_rho <- matrix(1, nrow = nrow(n), ncol = nrow(n))
theta <- cbind(theta_test, theta_test)
f_t <- step4fun(K_rho, theta)
