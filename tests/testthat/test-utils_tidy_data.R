# The test name completes the sentence 'Test that ... <function behavior>'
test_that("tidy_cleaned_trips_by_timestamp excludes user input columns when there are no user inputs.", {
  testthat::skip_on_ci()

  # Use the date range below within test-data to get empty user_inputs
  dates <- c("2015-08-23", "2015-08-24")
  queried_trips <- query_cleaned_trips_by_timestamp(cons, dates)
  tidied_trips <- queried_trips %>% tidy_cleaned_trips_by_timestamp(.)

  expect_true(is.data.table(tidied_trips))
  expect_true(nrow(tidied_trips) >= 1)
  expect_true(ncol(tidied_trips) >= 1)
})


test_that("summarise_trips_without_trips returns a nonempty data.table", {
  testthat::skip_on_ci()
  summary_df <-
    tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons)) %>%
    summarise_trips_without_trips(cons)

  expect_true(is.data.table(summary_df))
  expect_true(nrow(summary_df) >= 1)
  expect_true(ncol(summary_df) >= 1)
})

test_that("participants after summarise_trips_without_trips matches participants after summarise_trips", {
  testthat::skip_on_ci()

  trips <- tidy_cleaned_trips(query_cleaned_trips(cons),
    project_crs = get_golem_config("project_crs")
  )

  participants_using_trips <-
    tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons)) %>%
    summarise_trips(., trips)

  participants_using_db_trip_summary <-
    tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons)) %>%
    summarise_trips_without_trips(., cons)

  testthat::expect_snapshot(participants_using_trips)
  testthat::expect_snapshot(participants_using_db_trip_summary)

})

test_that("summarise_server_calls returns a nonempty data.table", {
  testthat::skip_on_ci()
  summary_df <-
    tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons)) %>%
    summarise_server_calls(cons)
  expect_true(is.data.table(summary_df))
  expect_true(nrow(summary_df) >= 1)
  expect_true(ncol(summary_df) >= 1)
})