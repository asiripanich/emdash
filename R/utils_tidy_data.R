tidy_participants <- function(stage_profiles, stage_uuids) {
  merge(x = stage_profiles,
        y = stage_uuids[, c("update_ts") := NULL],
        by = "user_id") %>%
    .[, c("uuid",
          "source",
          "device_token",
          "mpg_array",
          "curr_sync_interval") := NULL] %>%
    data.table::setcolorder(c("user_email", "user_id")) %>%
    .[, -"user_email"]
}

tidy_trip_ends <- function(trip_ends) {
  # unnesting the xml trip end survey result
  te <-
    trip_ends[, .(user_id, data.timestamp, data.start_ts, data.end_ts, survey = data.survey_result)]
  
  te_unnested = lapply(1:nrow(te), function(i) {
    unlist(te[i,]) %>% t() %>% as.data.table()
  }) %>%
    rbindlist(fill = TRUE) %>%
    setnames(gsub("trip-end-survey_v9.", "data_old.", names(.))) %>%
    .[, .SD, .SDcols = !grepl("data_old", names(.))] %>%
    setnames(gsub("survey.", "", names(.))) %>%
    .[!is.na(data.start)] %>%
    janitor::clean_names() %>%
    .[, -"data_version"]
  
  # clean up columns
  te_unnested %>%
    tidyr::unite(travel_mode, dplyr::contains("travel_mode"), sep = "|", na.rm = TRUE) %>%
    tidyr::unite(destination_purpose, dplyr::contains("purpose"), sep = "|", na.rm = TRUE) %>%
    tidyr::unite(transit_fees, dplyr::contains("transit_fees"), sep = "|", na.rm = TRUE) %>%
    tidyr::unite(parking_location, dplyr::contains("parking_location"), sep = "|", na.rm = TRUE) %>%
    tidyr::unite(n_persons, dplyr::contains("total_people_in_trip_party"), sep = "|", na.rm = TRUE) %>%
    tidyr::unite(toll_charges, dplyr::contains("toll_charges"), sep = "|", na.rm = TRUE) %>%
    tidyr::unite(parking_cost, dplyr::contains("parking_cost"), sep = "|", na.rm = TRUE) %>%
    tidyr::unite(non_household_members, dplyr::contains("non_household_member"), sep = "|", na.rm = TRUE) %>%
    dplyr::mutate(
      data_timestamp = lubridate::as_datetime(data_timestamp),
      data_start = lubridate::as_datetime(data_start),
      data_end = lubridate::as_datetime(data_end),
      data_start_ts = as.numeric(data_start_ts),
      data_end_ts = as.numeric(data_end_ts)
      # data_start = lubridate::as_datetime(data_start, tz = "Australia/Sydney"),
      # data_end = lubridate::as_datetime(data_end, tz = "Australia/Sydney")
    ) %>%
    as.data.table()
}

tidy_cleaned_trips = function(cleaned_trips) {
  cleaned_trips %>%
    dplyr::select(-dplyr::contains(c(".hour", ".second", ".minute", ".day", ".month", ".year", ".weekday"))) %>%
    setnames(gsub("data.", "", names(.))) %>%
    janitor::clean_names() %>%
    dplyr::select(-metakey, -metaplatform) %>%
    dplyr::mutate(
      start_fmt_time = lubridate::as_datetime(start_fmt_time),
      end_fmt_time = lubridate::as_datetime(end_fmt_time)
    ) %>%
    data.table::setDT()
}

complete_trips = function(tidied_cleaned_trips, tidied_trip_ends, project_crs) {
  
  smallest_rounding_digit = 2
  
  # select the most recent responses
  most_recent_trip_ends =
    tidied_trip_ends %>%
    .[, `:=`(data_start_ts = round(data_end_ts, smallest_rounding_digit),
             data_end_ts = round(data_end_ts, smallest_rounding_digit))] %>%
    .[order(data_timestamp), ] %>%
    .[, order := 1:.N, by = .(user_id, data_start_ts, data_end_ts)] %>%
    .[.[, .I[which.max(order)], by = .(user_id, data_start_ts, data_end_ts)]$V1]
  
  # round *_ts variables to make equality operators not to ignore a very small diff
  tidied_trips_rounded_ts =
    tidied_cleaned_trips %>%
    .[, `:=`(start_ts = round(start_ts, smallest_rounding_digit),
             end_ts = round(end_ts, smallest_rounding_digit))] %>%
    .[, end_ts_plus := end_ts + 5 * 60]
  
  # merge trips and end surveys using timestamps
  trips_dt =
    most_recent_trip_ends[tidied_trips_rounded_ts,
                          on = .(user_id,
                                 data_start_ts >= start_ts,
                                 data_end_ts <= end_ts_plus
                          ),
                          allow.cartesian = TRUE] %>%
    .[destination_purpose != "trip_not_valid", ]
  
  # merge non-validated trips (trips with no trip-end surveys)
  trips_dt =
    rbind(trips_dt,
          tidied_trips_rounded_ts[!raw_trip %in% trips_dt[, raw_trip],],
          fill = TRUE)
  
  # create a geometry column but is not a sf object yet
  trips_dt_with_geometry <-
    copy(trips_dt) %>%
    .[,
      geometry := list(list(st_linestring(matrix(
        as.numeric(unlist(
          c(start_loc_coordinates, end_loc_coordinates)
        )),
        ncol = 2,
        byrow = T
      )))),
      by = 1:nrow(.)] %>%
    # add lat and lon columns for both start and end locations
    dplyr::mutate(end_lat_lon = gsub("c\\(|\\)| ", "", paste(end_loc_coordinates)),
           start_lat_lon = gsub("c\\(|\\)| ", "", paste(start_loc_coordinates))) %>%
    tidyr::separate(end_lat_lon, into = c("end_lon", "end_lat"), sep = ',', convert = TRUE) %>%
    tidyr::separate(start_lat_lon, into = c("start_lon", "start_lat"), sep = ',', convert = TRUE)
  
  # further processing
  trips_dt_with_geometry =
    trips_dt_with_geometry %>%
    # aggregate similar labels
    dplyr::mutate(
      travel_mode = gsub('vehicle_driver', 'driver', travel_mode),
      travel_mode = gsub('vehicle_passenger', 'passenger', travel_mode),
      travel_mode = gsub('taxi_and_ride_sharing_services|^taxi$', 'taxi_and_ride', travel_mode),
      travel_mode = gsub('bike', 'bicycle', travel_mode),
      destination_purpose = dplyr::case_when(
        grepl("pick_up_or_dr", destination_purpose) ~ "pick_up_or_drop_off_someone",
        grepl("pick_up_or_del", destination_purpose) ~ "pick_up_or_delivery_something",
        grepl("recreation", destination_purpose) ~ "recreational (e.g exercise).",
        grepl("personal_busi", destination_purpose) ~ "Personal business",
        grepl("social", destination_purpose) ~ "Social (e.g. dine out)",
        grepl("accom", destination_purpose) ~ "Accompany someone",
        grepl("buy_something", destination_purpose) ~ "shopping",
        grepl("work", destination_purpose) ~ "Work related",
        TRUE ~ as.character(destination_purpose)
      ),
      destination_purpose = gsub("_", " ", destination_purpose)
    ) %>%
    dplyr::mutate(
      transit_fees = as.numeric(transit_fees),
      toll_charges = as.numeric(toll_charges),
      parking_cost = as.numeric(parking_cost)
    ) %>%
    data.table::setDT(.)
  
  # turn it into a sf object
  trips_sf =
    sf::st_sf(trips_dt_with_geometry[, -"geometry"],
          geometry = trips_dt_with_geometry[["geometry"]],
          crs = project_crs)
  
  trips_sf %>%
    dplyr::select(-dplyr::starts_with(c("meta", "data_")), -order) %>%
    dplyr::select(-dplyr::ends_with(c("_ts", "_ts_plus", "_place",
                                      "_loc_type", "raw_trip",
                                      "_lat", "_lon",
                                      "_coordinates"))) %>%
    dplyr::select(user_id,
                  start_fmt_time, start_local_dt_timezone,
                  end_fmt_time, end_local_dt_timezone,
                  dplyr::everything())
  
}

#' Convert columns to datetime
#'
#' @param .data a data.table object
#' @param cols the names of the columns to convert to datetime
#' @param tz time zone default as "Australia/Sydney".
#'
#' @return .data
#' @export
convert_columns_to_datetime <-
  function (.data, cols, tz = "Australia/Sydney")
  {
    stopifnot(data.table::is.data.table(.data))
    .data[, `:=`(c(cols), lapply(.SD, function(.x) {
      lubridate::as_datetime(.x, tz = tz)
    })), .SDcols = cols]
  }
