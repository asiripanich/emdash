summarise_trips = function(participants, trips) {
  summ_trips <-
    trips %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      date = lubridate::date(start_fmt_time)
    ) %>%
    dplyr::group_by(user_id) %>%
    dplyr::summarise(
      n_trips = dplyr::n(),
      n_trips_today = sum(date == Sys.Date()),
      n_trip_ends = sum(!is.na(travel_mode)),
      n_active_days = data.table::uniqueN(date),
      first_trip_datetime = min(start_fmt_time),
      last_trip_datetime = max(start_fmt_time),
      n_days = as.numeric(difftime(last_trip_datetime, first_trip_datetime, units = "days"))
    )
  
  merge(participants, summ_trips, by = "user_id", all.x = TRUE)
}
