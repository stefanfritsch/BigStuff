library(testthat)
library(data.table)
library(magrittr)

context("drop_in_place")

#' Test drop by .n
#' ===============
test_that("drop by .n works", {
  DT <- as.data.table(iris)
  DT[,ID:=1:.N]

  A <- copy(DT)
  drop_in_place(A, 10)
  setattr(A, "index", NULL)

  expect_equal(A, DT[-(1:10)])
})

#' Test drop by .keep
#' ==================
test_that("drop by .keep works", {
  DT <- as.data.table(iris)
  DT[,ID:=1:.N]

  A <- copy(DT)
  drop_in_place(A, .keep = Species == "virginica")
  # setattr(A, "index", NULL)

  expect_equal(A, DT[Species=="virginica"])

})
