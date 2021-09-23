test_that("get_n_trips_in_query is empty when the dates are the same", {
  dates <- c("2016-08-05", "2016-08-05")
  expect_true(is.null(get_n_trips_in_query(cons, dates)))
})

test_that("get_n_trips_in_query is empty when there are no documents in the date range", {
  dates <- c("2015-09-20", "2015-09-30")
  expect_true(is.null(get_n_trips_in_query(cons, dates)))
})

test_that("get_n_trips_in_query is numeric when there are documents in the date range", {
  dates <- c("2016-08-05", "2016-08-11")
  expect_true(is.numeric(get_n_trips_in_query(cons, dates)))
})

# test load_allowed
