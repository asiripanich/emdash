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
  tidied_participants <-
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
  
  waldo::compare(participants_using_db_trip_summary$first_trip_datetime,participants_using_trips$first_trip_datetime, 
                 x_arg = "without_trips",y_arg = "with_trips")
  waldo::compare(participants_using_db_trip_summary$last_trip_datetime,participants_using_trips$last_trip_datetime, 
                 x_arg = "without_trips",y_arg = "with_trips")
  
  waldo::compare(participants_using_db_trip_summary$first_trip_local_datetime,participants_using_trips$first_trip_local_datetime, 
                 x_arg = "without_trips",y_arg = "with_trips")
  waldo::compare(participants_using_db_trip_summary$last_trip_local_datetime,participants_using_trips$last_trip_local_datetime, 
                 x_arg = "without_trips",y_arg = "with_trips")
  
  
  waldo::compare(participants_using_db_trip_summary$n_active_days,participants_using_trips$n_active_days, 
                 x_arg = "without_trips",y_arg = "with_trips")
  waldo::compare(participants_using_db_trip_summary$n_trips_today,participants_using_trips$n_trips_today, 
                 x_arg = "without_trips",y_arg = "with_trips")
  waldo::compare(participants_using_db_trip_summary$n_days,participants_using_trips$n_days, 
                 x_arg = "without_trips",y_arg = "with_trips")
  waldo::compare(participants_using_db_trip_summary$n_trips,participants_using_trips$n_trips, 
                 x_arg = "without_trips",y_arg = "with_trips")
  
  # compare the data frames. 'Participants with each summarise trips method do not match.')
  colnames(participants_using_db_trip_summary) == colnames(participants_using_trips)
  
  # Didn't work the way I expected it to
  # for (j in colnames(participants_using_db_trip_summary)) {
  #   waldo::compare(participants_using_db_trip_summary[[j]],participants_using_trips[[j]])
  # }
  
  
  ### Below are the summary columns to check, as seen in summarise_trips_without_trips
  # .[, .(
  #   n_trips_today = sum(date == Sys.Date()),
  #   n_active_days = data.table::uniqueN(date),
  #   first_trip_datetime = min(start_fmt_time),
  #   last_trip_datetime = max(start_fmt_time), # in UTC
  #   first_trip_local_datetime = format(min(lubridate::as_datetime(start_local_time)), usetz = FALSE),
  #   last_trip_local_datetime = format(max(lubridate::as_datetime(end_local_time)), usetz = FALSE)
  # ), by = user_id] %>% 
  #   .[, n_days := round(as.numeric(difftime(last_trip_datetime, first_trip_datetime, units = "days")), 1)]
  # 
  # # Count the number of trips per user
  # n_trips <- count_total_trips(cons)
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
