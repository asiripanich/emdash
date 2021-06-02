test_that("tidy_*", {
  testthat::skip_on_ci()

  cons <- connect_stage_collections()

  tidied_cleaned_trips <-
    query_cleaned_trips(cons) %>%
    tidy_cleaned_trips()

  tidied_participants <-
    tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons))

  summary_df <- summarise_trips(tidied_participants, tidied_cleaned_trips)

  expect_true(is.data.table(summary_df))
  expect_true(nrow(summary_df) >= 1)
  expect_true(ncol(summary_df) >= 1)
})
