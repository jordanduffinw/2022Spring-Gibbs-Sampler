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
  expect_equal(ncol(Z), length(rho))
})

test_that("rho is a vector", {
  expect_equal(TRUE, is.vector(rho))
})

test_that("rho has the correct length", {
  expect_equal(length(rho), D)
})


#create input object theta, from GibbsSampler-method.R, lines 44 and 83
#groups <- nrow(y)
#theta <- matrix(nrow = groups, ncol = 2)

test_that("theta is of correct type", {
  expect_equal(TRUE, is.matrix(theta))
})

test_that("theta has the correct number of rows", {
  expect_equal(nrow(n), nrow(theta))
})

test_that("theta has the correct number of columns", {
  expect_equal(2, ncol(theta))
})

#Create step4fun output object f_t
#f_t <- step4fun(Z, rho, theta, sigma2_theta = 1)

test_that("f_t is of correct type", {
  expect_equal(TRUE, is.vector(f_t))
})

test_that("f_t has the correct number of rows", {
  expect_equal(1, nrow(f_t))
})

test_that("f_t has the correct number of columns", {
  expect_equal(1, ncol(f_t))
})


