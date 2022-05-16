test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


#Used wrapperfun_pseudo.R to get all the objects for these tests.
#Obejcts from wrapperfun_psuedo.R are: y, n, j
source("wrapperfun_pseudo.R")

#create w_j for step2fun input
#w_j <- step1fun(y, n, j)[,1]


test_that("X has the correct number of rows", {
  expect_equal(nrow(X), nrow(y))
})

test_that("w_j is the correct length", {
  expect_equal(length(w_j), nrow(X))
})

#create k_j for step2fun input
#k_j is "A vector of length N where each element is \eqn{\kappa_{ij} = y_{ij}-n_{ij}/2}"
#Also note that k_j is (y-n)/2 and not y-(n/2)
#k_j <- as.vector(unlist((y[,1] - n)/2))

test_that("k_j is a vector", {
  expect_equal(TRUE, is.vector(k_j))
})

test_that("k_j is the correct length", {
  expect_equal(length(k_j), nrow(X))
})

#k_j is a vector, but y and n are dataframes
#therefore to compare the result of y-n/2 needs to be unlisted and then turned into a vector
test_that("k_j has the correct values", {
  expect_equal(k_j, as.vector(unlist((y[,1] - n)/2)))
})

#Create Lambda as an input for step2fun
#Lambda <- 0.1

test_that("Lambda is correct value", {
  expect_equal(Lambda, 0.1)
})

test_that("Lambda is correct length", {
  expect_equal(1, length(Lambda))
})

test_that("beta_tilde_j is correct length", {
  expect_equal(2, length(step2fun(X, w_j, k_j)))
})

test_that("beta_tilde_j is correct type: vector", {
  expect_equal(TRUE, is.vector(step2fun(X, w_j, k_j)))
})

