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

test_that("test GetEBXCodeLists", {
  codelists <- GetEBXCodeLists()
  expect_true(!is.null(codelists),"retrieving codelists object; is null")
  expect_true(nrow(codelists)>2,"retrieving codelists object; no rows")

  expect_true(!exists(codelists$Identifier),"codelists Identifier")
  expect_true(!exists(codelists$Acronym),"codelists Acronym")
  expect_true(!exists(codelists$Folder),"codelists Folder")
  expect_true(!exists(codelists$Name),"codelists Name")
  expect_true(!exists(codelists$Branch),"codelists Branch")
  expect_true(!exists(codelists$Instance),"codelists Instance")
})

test_that("test GetEBXGroups", {
  groups <- GetEBXGroups()
  expect_true(!is.null(groups),"retrieving groups object; is null")
  expect_true(nrow(groups)>2,"retrieving groups object; no rows")

  expect_true(!exists(groups$Identifier),"groups Identifier")
  expect_true(!exists(groups$Acronym),"groups Acronym")
  expect_true(!exists(groups$Acronym),"groups from")
  expect_true(!exists(groups$Acronym),"groups to")
  expect_true(!exists(groups$Folder),"groups Folder")
  expect_true(!exists(groups$Name),"groups Name")
  expect_true(!exists(groups$Branch),"groups Branch")
  expect_true(!exists(groups$Instance),"groups Instance")
})






