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

complete_trips = function(tidied_cleaned_trips, project_crs) {
  
  smallest_rounding_digit = 2
  
  # select the most recent responses
  # SHANKARI: pretend that there are no responses
  most_recent_trip_ends <- vector()
  
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
