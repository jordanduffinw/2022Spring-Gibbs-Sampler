test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

#Used wrapperfun_pseudo.R to get all the objects for these tests.
#Obejcts from wrapperfun_psuedo.R are: y, n, j
source("wrapperfun_pseudo.R")

test_that("n has the correct number of rows", {
  expect_equal(nrow(n), nrow(y))
})

test_that("J_items is the correct length", {
  expect_equal(j, ncol(y))
})

test_that("step1fun output matrix dimensions are correct", {
  expect_equal(dim(step1fun(y = y, n = n, J_items = j)), dim(y))
})

test_that("step1fun output matrix number of rows are correct", {
  expect_equal(dim(step1fun(y = y, n = n, J_items = j))[1], dim(n)[1])
})

test_that("step1fun output matrix number of rows are correct", {
  expect_equal(nrow(step1fun(y = y, n = n, J_items = j)), nrow(n))
})

test_that("step1fun output matrix number of rows are correct", {
  expect_equal(nrow(step1fun(y = y, n = n, J_items = j)), nrow(y))
})

test_that("step1fun output matrix number of columns are correct", {
  expect_equal(dim(step1fun(y = y, n = n, J_items = j))[2], j)
})

test_that("step1fun output matrix number of columns are correct", {
  expect_equal(ncol(step1fun(y = y, n = n, J_items = j)), ncol(y))
})

test_that("y is correct type of object", {
  expect(is.data.frame(y), "y is not a data.frame")
})

test_that("n is correct type of object", {
  expect(is.data.frame(n), "n is not a data.frame")
})

test_that("J_items is correct type of object", {
  expect(is.integer(j), "J_items is not an integer")
})

test_that("step1fun output is correct type of object", {
  expect(is.matrix(step1fun(y = y, n = n, J_items = j)), "step1fun is not a matrix")
})

