library(testthat)
context("faoebx5")

test_that("test ReadEBXCodeList", {
  fao_level1 <- ReadEBXCodeList('CL_FI_COMMODITY_FAO_LEVEL1')

  expect_true(!is.null(fao_level1),"retrieving groups object; is null")
  expect_true(nrow(fao_level1)>2,"retrieving groups object; no rows")
  expect_true(ncol(fao_level1)==5,"retrieving dims object; not 5 columns")

  expect_true(!exists(fao_level1$FAO_Code),"fao_level1$FAO_Code")
})

test_that("test ReadEBXGroup", {
  level1_level2 <- ReadEBXGroup('HCL_FI_COMMODITY_FAOL1_FAOL2')

  expect_true(!is.null(level1_level2),"retrieving groups object; is null")
  expect_true(nrow(level1_level2)>2,"retrieving groups object; no rows")
  expect_true(ncol(level1_level2)==2,"retrieving dims object; not 2 columns")

  expect_true(!exists(level1_level2$Member),"level1_level2$Member")
})

test_that("test ReadMetadata", {
  metadata <- ReadMetadata()
  dims <- GetDatasetDimensions(metadata, datasetID = 1)

  expect_true(!is.null(dims),"retrieving dims object; is null")
  expect_true(nrow(dims)==4,"retrieving dims object; not 4 rows")
  expect_true(ncol(dims)==7,"retrieving dims object; not 7 columns")
})

test_that("test GetEBXHierarchy", {
  hierarchy <- GetEBXHierarchy('CL_FI_SPECIES_MAJOR', 'CL_FI_SPECIES_ITEM')

  expect_true(!is.null(hierarchy),"retrieving hierarchy object; is null")
  expect_true(nrow(hierarchy)>12000,"retrieving hierarchy object; not >12000 rows")
  expect_true(ncol(hierarchy)==2,"retrieving hierarchy object; not 2 columns")
})
