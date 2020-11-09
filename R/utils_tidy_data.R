#' Create a tidied participant data.table
#' 
#'
#' @param stage_profiles the output of `query_stage_profiles()`.
#' @param stage_uuids the output of `query_stage_uuids()`.
#'
#' @note 
#' 
#' the `update_ts` field can be used to infer the signup datetime of each user.
#' 
#' @return a data.table
#' @export
tidy_participants <- function(stage_profiles, stage_uuids) {
  merged <- merge(x = stage_profiles,
        y = stage_uuids[, c("update_ts") := NULL],
        by = "user_id")
  #print(names(merged))
  max_cols_to_purge <- c("uuid", "source", "device_token", "mpg_array", "curr_sync_interval")
  cols_to_purge <- intersect(names(merged), max_cols_to_purge)
  #print(cols_to_purge)
  merged %>%
    .[, eval(cols_to_purge) := NULL] %>%
    data.table::setcolorder(c("user_email", "user_id")) %>%
    .[, -"user_email"]
}

#' Tidy the 'cleaned trips' data.frame into a sf object.
#'
#' @param cleaned_trips a data.table output from `query_cleaned_trips()`.
#' @param project_crs a EPSG code. Default as 4326.
#' @param smallest_rounding_digit an integer value.
#'
#' @return a spatial data.frame of class sf. 
#' @export
tidy_cleaned_trips = function(cleaned_trips, project_crs = 4326, smallest_rounding_digit = 2) {
  message("Finished query, about to clean trips")
  cleaned_trips_sf =
    # flatten out names and remove unnecessary columns
    cleaned_trips %>%
    dplyr::select(-dplyr::contains(
      c(
        ".hour",
        ".second",
        ".minute",
        ".day",
        ".month",
        ".year",
        ".weekday"
      )
    )) %>%
    setnames(gsub("data.", "", names(.))) %>%
    janitor::clean_names() %>%
    dplyr::select(-metakey, -metaplatform) %>%
    dplyr::mutate(
      start_fmt_time0 = start_fmt_time,
      end_fmt_time0 = end_fmt_time,
      start_fmt_time = lubridate::as_datetime(start_fmt_time0),
      end_fmt_time = lubridate::as_datetime(end_fmt_time0),
      start_local_time = purrr::map2(start_fmt_time, start_local_dt_timezone, ~format(.x, tz = .y, usetz = TRUE)),
      end_local_time = purrr::map2(end_fmt_time, end_local_dt_timezone, ~format(.x, tz = .y, usetz = TRUE))
    ) %>%
    tidyr::unnest(c("start_local_time", "end_local_time")) %>%
    # convert to data.table for speed!
    data.table::setDT(.) %>%
    # create a geometry column but is not a sf object yet
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
    dplyr::mutate(
      end_lat_lon = gsub("c\\(|\\)| ", "", paste(end_loc_coordinates)),
      start_lat_lon = gsub("c\\(|\\)| ", "", paste(start_loc_coordinates))
    ) %>%
    tidyr::separate(
      end_lat_lon,
      into = c("end_lon", "end_lat"),
      sep = ',',
      convert = TRUE
    ) %>%
    tidyr::separate(
      start_lat_lon,
      into = c("start_lon", "start_lat"),
      sep = ',',
      convert = TRUE
    ) %>% 
    # convert into a sf object, this allows us to use spatial mapping functions
    # from packages like `ggplot2` and `mapview`. 
    # this requires the curley brackets because we are re-using the placeholder
    # see: the section 'Re-using the placeholder for attributes' in
    # https://magrittr.tidyverse.org/
    {
      sf::st_sf(dplyr::select(., -geometry),
                geometry = .[["geometry"]],
                crs = project_crs)
    } %>%
    dplyr::select(-dplyr::starts_with(c("meta", "data_"))) %>%
    dplyr::select(-dplyr::ends_with(
      c(
        "_ts",
        "_ts_plus",
        "_place",
        "_loc_type",
        "raw_trip",
        "_lat",
        "_lon"
        #"_coordinates" # keep the start and end coordinates for later visualization
      )
    )) %>%
    dplyr::select(
      user_id,
      start_fmt_time0,
      start_local_dt_timezone,
      start_fmt_time,
      start_local_time,
      end_fmt_time0,
      end_local_dt_timezone,
      end_fmt_time,
      end_local_time,
      dplyr::everything()
    ) %>%
    # converting duration in seconds and distance in metres
    dplyr::mutate(
      duration_min = round(duration/60, smallest_rounding_digit),
      distance_mi = round(distance/1609, smallest_rounding_digit),
      distance_km = round(distance/1000, smallest_rounding_digit)
    )
  message("Finished cleaning trips")
  return(cleaned_trips_sf)
}

#' Create a summary of trips in data.table format.
#'
#' @param participants the output from `tidy_participants()`.
#' @param trips the output from `tidy_cleaned_trips()`.
#'
#' @return a data.table.
#' @export
summarise_trips = function(participants, trips) {
  summ_trips <-
    trips %>%
    sf::st_drop_geometry(.) %>%
    data.table::setDT(.) %>%
    .[, date := lubridate::date(lubridate::as_datetime(start_local_time))] %>% # adds the date of local datetime of trip
    .[, .(
      n_trips = .N, 
      n_trips_today = sum(date == Sys.Date()), # compares date of local datetime of trip to date of local time of dashboard user
      n_active_days = data.table::uniqueN(date),
      first_trip_datetime = min(start_fmt_time), # in UTC
      last_trip_datetime = max(start_fmt_time), # in UTC
      first_trip_local_datetime = format(min(lubridate::as_datetime(start_local_time)), usetz = TRUE),
      last_trip_local_datetime = format(max(lubridate::as_datetime(end_local_time)), usetz = TRUE)
    ), by = user_id] %>%
    .[, n_days := round(as.numeric(difftime(last_trip_datetime, first_trip_datetime, units = "days")), 1)]
  
  merge(participants, summ_trips, by = "user_id", all.x = TRUE)
}


#' Tidy the 'cleaned locations' data.frame into a tibble.
#'
#' @param cleaned_locations a data.table output from `query_cleaned_trips()`.
#'
#' @return a tibble. 
#' @export
tidy_cleaned_locations = function(cleaned_locations) {
  message("Finished query, about to clean locations")
  tidied_locations =
    # flatten out names and select specific columns
    cleaned_locations %>%
    setnames(gsub("data.", "", names(.))) %>%
    janitor::clean_names() %>%
    dplyr::select(user_id, loc_type, longitude, latitude, loc_coordinates, fmt_time, idx, mode, section) %>%
    # convert datetime
    dplyr::mutate(
      fmt_time_utc = lubridate::as_datetime(fmt_time)
    ) 
  message("Finished cleaning locations")
  return(tidied_locations)
}


#' Tidy together the 'tidied trips' and 'tidied locations' to generate trajectories within a trip
#'
#' @param tidied_trips a data.frame from `tidy_cleaned_trips()`.
#' @param tidied_locations a tibble from `tidy_cleaned_locations()`.
#' @param project_crs a EPSG code. Default as 4326.
#'
#' @return a data.frame of class sf. 
#' @export
generate_trajectories = function(tidied_trips, tidied_locations, project_crs = 4326) {
  message("About to generate trajectories")
  
  # drop trip geometries (just a line between OD) and set trips as data.table
  tidied_trips <- tidied_trips %>% sf::st_drop_geometry()
  data.table::setDT(tidied_trips)
  # set locations as data.table and duplicate location time to set an "interval"
  data.table::setDT(tidied_locations)[, fmt_time_utc_end := fmt_time_utc] 
  # join locations to trips according to time interval overlap
  data.table::setkey(tidied_locations, user_id, fmt_time_utc, fmt_time_utc_end)
  data.table::setkey(tidied_trips, user_id, start_fmt_time, end_fmt_time)
  joined_trips_and_locs <- data.table::foverlaps(tidied_locations, tidied_trips)
  
  # convert locations to points
  trajectories <- joined_trips_and_locs %>%
    dplyr::mutate(location_points = purrr::map(loc_coordinates, sf::st_point)) %>%
    sf::st_sf(crs = project_crs) %>%
  # cast linestrings between points of the same trips
    dplyr::group_by(dplyr::across(colnames(tidied_trips))) %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING")
  
  message("Trajectories created")
  return(trajectories)
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
