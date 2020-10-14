test_that("tidy_*", {
  
  testthat::skip_on_ci()
  
  cons = connect_stage_collections()
  
  tidied_cleaned_trips = 
    query_cleaned_trips(cons) %>%
    tidy_cleaned_trips()
  
  tidied_participants = 
    tidy_participants(query_stage_profiles(cons), query_stage_uuids(cons))
  
  checkmate::expect_data_table(summarise_trips(tidied_participants, tidied_cleaned_trips),
                               min.rows = 1)
  
})
