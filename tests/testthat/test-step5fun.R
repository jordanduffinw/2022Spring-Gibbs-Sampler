test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

#Used wrapperfun_pseudo.R to get all the objects for these tests.
#Obejcts from wrapperfun_psuedo.R are: y, n, j
source("wrapperfun_pseudo.R")


test_that("Z is a matrix", {
  expect_equal(TRUE, is.matrix(Z))
})

test_that("Z has the correct number of rows", {
  expect_equal(nrow(Z), nrow(n))
})

test_that("Z has the correct number of columns", {
  expect_equal(ncol(Z), D)
})

f <- step4fun(Z, rho, theta, sigma2_theta = 1 )

test_that("f is a vector", {
  expect_equal(TRUE, is.vector(f))
})

test_that("f has the correct length", {
  expect_equal(length(f), nrow(n))
})

test_that("f has the correct length", {
  expect_equal(length(f), nrow(Z))
})

test_that("rho_lag is a vector", {
  expect_equal(TRUE, is.vector(rho_lag))
})

test_that("rho_lag has the correct length", {
  expect_equal(length(rho_lag), D)
})

test_that("rho_lag has the correct length", {
  expect_equal(length(rho_lag), ncol(Z))
})

test_that("Sigma_rho is a matrix", {
  expect_equal(TRUE, is.matrix(Sigma_rho))
})

test_that("Sigma_rho has the correct number of rows", {
  expect_equal(nrow(Sigma_rho), D)
})

test_that("Sigma_rho has the correct number of rows", {
  expect_equal(nrow(Sigma_rho), ncol(Z))
})

test_that("Sigma_rho has the correct number of columns", {
  expect_equal(ncol(Sigma_rho), D)
})

test_that("Sigma_rho has the correct number of columns", {
  expect_equal(ncol(Sigma_rho), ncol(Z))
})

test_that("a has the correct length", {
  expect_equal(1, length(a))
})

test_that("b has the correct length", {
  expect_equal(1, length(b))
})

test_that("output rho is a vector", {
  expect_equal(TRUE, is.vector(step5fun(Z, f, rho_lag, Sigma_rho, a, b)))
})

test_that("output rho has the correct length", {
  expect_equal(D, length(step5fun(Z, f, rho_lag, Sigma_rho, a, b)))
})

test_that("output rho has the correct length", {
  expect_equal(ncol(Z), length(step5fun(Z, f, rho_lag, Sigma_rho, a, b)))
})
