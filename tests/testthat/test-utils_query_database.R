context("Test query functions")
cons <- connect_stage_collections()

test_that("get_query_size is empty when the dates are the same", {
  testthat::skip_on_ci()
  dates <- c('2015-08-23','2015-08-23')
  expect_true(is.null(get_query_size(cons,dates)))
})

test_that("get_query_size is empty when there are no documents in the date range", {
  testthat::skip_on_ci()
  dates <- c('2015-09-20','2015-09-30')
  expect_true(is.null(get_query_size(cons,dates)))
})

test_that("get_query_size is numeric when there are documents in the date range", {
  testthat::skip_on_ci()
  dates <- c('2015-08-23','2015-08-24')
  expect_true(is.numeric(get_query_size(cons,dates)))
})

# test load_allowed
