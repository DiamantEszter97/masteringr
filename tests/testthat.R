
# devtools::install_github("DiamantEszter97/masteringr")
library(testthat)
library(masteringr)

context("Forint")

test_check("masteringr")
devtools::test()


test_that("forint returns value correctly", {

  expect_equal(forint(42), "42 HUF")
  expect_equal(forint(21), "21 HUF")
  expect_equal(forint(4582648), "4,582,648 HUF")

})


test_that("forint gives back correct output", {

  expect_output(str(forint(42)), 'chr "42 HUF"')
  expect_output(str(forint(21)), 'chr "21 HUF"')
  expect_output(str(forint(4582648)), 'chr "4,582,648 HUF"')

})


test_that("forint gives back correct type", {

  expect_is(forint(42), "character")
  expect_is(forint(21), "character")
  expect_is(forint(4582648), "character")

})


test_that("forint fails when string is given as argument",{

  expect_error(forint("42"), "Assertion on 'x' failed: Must be of type 'number', not 'character'.")
  expect_error(forint("21"), "Assertion on 'x' failed: Must be of type 'number', not 'character'.")
  expect_error(forint("4582648"), "Assertion on 'x' failed: Must be of type 'number', not 'character'.")


})


test_that("forint fails when multiples arguments are given",{

  expect_error(forint(42,5), "unused argument")
  expect_error(forint(21,6), "unused argument")
  expect_error(forint(4582648,5642), "unused argument")


})


# failed on the following:
test_that("forint adds 0 at the end of non-integers and only one digit is given after dot",{

  expect_equal(forint(42.5), "42.50 HUF")
  expect_equal(forint(21.8), "21.80 HUF")
  expect_equal(forint(4582648.3), "4,582,648.30 HUF")

})


# failed on the following for the same reason as previous:
test_that("forint does not add 0 at the end of non-integers and only one digit is given after dot",{

  expect_equal(forint(42.52), "42.52 HUF")
  expect_equal(forint(21.88), "21.88 HUF")
  expect_equal(forint(4582648.31), "4,582,648.31 HUF")

})


