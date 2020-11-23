library(testthat)
context("faoebx5")

test_that("test InsertEBXCodeList", {
  data_insert <- data.frame(
    Identifier = 999,
    Acronym = 'TEST_CSV',
    EBX_Codelist = 200
  )
  result <- InsertEBXCodeList(data= data_insert, sdmx_codelist_name  = 'CSV_METADATA_CONCEPT')
  expect_true(result == 'OK')
})


test_that("test UpdateEBXCodeList", {
  cl_update <- data.frame(
    Identifier = 999,
    Acronym = 'TEST_CSV',
    EBX_Codelist = 200
  )
  result <- UpdateEBXCodeList(data= cl_update, sdmx_codelist_name  = 'CSV_METADATA_CONCEPT')
  expect_true(result == 'OK')
})

test_that("test RemoveEBXCodeList", {
  cl_delete <- data.frame(
    Identifier = 999
  )
  result <- RemoveEBXCodeList(data= cl_delete, sdmx_codelist_name  = 'CSV_METADATA_CONCEPT')
  expect_true(result == 'OK')
})
