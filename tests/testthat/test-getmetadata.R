library(testthat)
context("faoebx5")

test_that("test getEbxConnection", {
  connection <- getEbxConnection()

  expect_true(!is.null(connection),"retrieving connection object")

  expect_true(!exists(connection$username),"connection username")
  expect_true(!exists(connection$password),"connection password")
  expect_true(!exists(connection$meta_branch),"connection meta_branch")
  expect_true(!exists(connection$meta_instance),"connection meta_instance")
  expect_true(!exists(connection$ebx_soap_url),"connection ebx_soap_url")
})


