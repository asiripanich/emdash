test_that("tidy_*", {
  testthat::skip_on_ci()

  cons <- connect_stage_collections(getOption('emdash.mongo_url'))

  # Use the date range below within test-data to get empty user_inputs
  dates <- c('2015-08-23','2015-08-27')
  tidied_cleaned_trips <-
    query_cleaned_trips_by_timestamp(cons,dates)

  tidied_participants <-
    tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons))
  
  # Make sure tidy_cleaned_trips_by_timestamp generated all user_input fields.
  trip_user_inputs <- 'mode_confirm' %in% colnames(tidied_cleaned_trips)
                    # should the check for all user_input columns be wrapped in one check?
  expect_true(trip_user_inputs, 'tidy_cleaned_trips_by_timestamp did not generate all user_input fields')

  summary_df <- summarise_trips_without_trips(tidied_participants,cons)

  expect_true(is.data.table(summary_df))
  expect_true(nrow(summary_df) >= 1)
  expect_true(ncol(summary_df) >= 1)
})

# Some starter tests:
# query_cleaned_trips_by_timestamp
#       tidy_cleaned_trips_by_timestamp
#       make sure it handles when there are no user inputs
# save a dataframe for testing tidy_cleaned_trips_by_timestamp?
# test load_allowed and get_query_size when there is only one day selected and no trips

# test specifically whether a function does what it is supposed to?

