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
  merged <- merge(
    x = stage_profiles,
    y = stage_uuids[, c("update_ts") := NULL],
    by = "user_id"
  )
  # print(names(merged))
  max_cols_to_purge <- c("uuid", "source", "device_token", "mpg_array", "curr_sync_interval")
  cols_to_purge <- intersect(names(merged), max_cols_to_purge)
  # print(cols_to_purge)
  merged %>%
    .[, eval(cols_to_purge) := NULL] %>%
    data.table::setcolorder(c("user_email", "user_id"))
}

#' Tidy the 'cleaned trips' data.frame into a sf object.
#'
#' @param cleaned_trips a data.table output from `query_cleaned_trips()`.
#' @param project_crs a EPSG code. Default as 4326.
#' @param smallest_rounding_digit an integer value.
#'
#' @return a spatial data.frame of class sf.
#' @export
tidy_cleaned_trips <- function(cleaned_trips, project_crs = 4326, smallest_rounding_digit = 2) {
  message("Finished query, about to clean trips")
  cleaned_trips_sf <-
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
    setnames(gsub("user_input", "", names(.))) %>%
    janitor::clean_names() %>%
    dplyr::select(-metakey, -metaplatform) %>%
    dplyr::mutate(
      start_fmt_time0 = start_fmt_time,
      end_fmt_time0 = end_fmt_time,
      start_fmt_time = lubridate::as_datetime(start_fmt_time0), # converts to UTC
      end_fmt_time = lubridate::as_datetime(end_fmt_time0), # converts to UTC
      start_local_time = purrr::map2(start_fmt_time, start_local_dt_timezone, ~ format(.x, tz = .y, usetz = TRUE)), # a timestamp but has to be a string
      end_local_time = purrr::map2(end_fmt_time, end_local_dt_timezone, ~ format(.x, tz = .y, usetz = TRUE)) # a timestamp but has to be a string
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
      by = 1:nrow(.)
    ] %>%
    # add lat and lon columns for both start and end locations
    dplyr::mutate(
      end_lat_lon = gsub("c\\(|\\)| ", "", paste(end_loc_coordinates)),
      start_lat_lon = gsub("c\\(|\\)| ", "", paste(start_loc_coordinates))
    ) %>%
    tidyr::separate(
      end_lat_lon,
      into = c("end_lon", "end_lat"),
      sep = ",",
      convert = TRUE
    ) %>%
    tidyr::separate(
      start_lat_lon,
      into = c("start_lon", "start_lat"),
      sep = ",",
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
        crs = project_crs
      )
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
        # "_coordinates" # keep the start and end coordinates for later visualization
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
      duration_min = round(duration / 60, smallest_rounding_digit),
      distance_mi = round(distance / 1609, smallest_rounding_digit),
      distance_km = round(distance / 1000, smallest_rounding_digit)
    )
  message("Finished cleaning trips")
  return(cleaned_trips_sf)
}

tidy_cleaned_trips_by_timestamp <- function(df) {
  # Within trips, metadata and data are list columns with multiple fields.
  # user_input is a list column found in data

  # Make a data table out of user_id and meta data.
  dt <- df[, c("user_id", "metadata")] %>% as.data.table()

  # Make a dataframe containing just the 'data' list-column
  data_df <- df[, c("data")]
  data_df_without_user_input <- data_df[, !"user_input" == names(data_df)]
  user_input_df <- df[, c("data")][, "user_input"] %>% as.data.table()

  # Combine the user_id/metadata data table with the 'data' dataframe.
  dt <- cbind(dt, data_df_without_user_input)

  # If there are no user inputs, only return dt without user input columns
  if (nrow(user_input_df) == 0) {
    message("There are no user inputs in the time range")
    return(dt)
  }
  cbind(dt, user_input_df)
}

summarise_trips_without_trips <- function(participants, cons) {
  date_query <- query_trip_dates(cons)
  # Generate relevant date related information from the query
  trip_dates <- lubridate::as_date(date_query$data$start_fmt_time) # converts to UTC
  start_fmt_time <- lubridate::as_datetime(date_query$data$start_fmt_time) # converts to UTC
  end_fmt_time <- lubridate::as_datetime(date_query$data$end_fmt_time) # converts to UTC

  start_local_time <- purrr::map2(start_fmt_time, date_query$data$start_local_dt$timezone, ~ format(.x, tz = .y, usetz = TRUE)) %>%
    as.character()
  end_local_time <- purrr::map2(end_fmt_time, date_query$data$end_local_dt$timezone, ~ format(.x, tz = .y, usetz = TRUE)) %>%
    as.character() # a timestamp but has to be a string

  date_dt <- cbind(date_query, trip_dates) %>%
    .[, !names(.) %in% c("data")] %>%
    as.data.table() %>%
    normalise_uuid()
  first_trip_local_datetime <- format(min(lubridate::as_datetime(start_local_time)), usetz = FALSE)

  summ_trips <-
    date_dt %>%
    .[, date := trip_dates] %>%
    # adds the date of local datetime of trip
    .[, .(
      n_trips_today = sum(date == Sys.Date()),
      n_active_days = data.table::uniqueN(date),
      first_trip_datetime = min(start_fmt_time),
      last_trip_datetime = max(start_fmt_time), # in UTC
      first_trip_local_datetime = format(min(lubridate::as_datetime(start_local_time)), usetz = FALSE),
      last_trip_local_datetime = format(max(lubridate::as_datetime(end_local_time)), usetz = FALSE)
    ), by = user_id] %>%
    .[, n_days := round(as.numeric(difftime(last_trip_datetime, first_trip_datetime, units = "days")), 1)]

  n_trips <- count_total_trips(cons)
  summ_trips <- merge(n_trips, summ_trips, by = "user_id")

  message("merging trip summaries with participants")
  merge(participants, summ_trips, by = "user_id", all.x = TRUE)
}

#' Create a summary of trips in data.table format.
#'
#' @param participants the output from `tidy_participants()`.
#' @param trips the output from `tidy_cleaned_trips()`.
#'
#' @return a data.table.
#' @export
summarise_trips <- function(participants, trips) {
  summ_trips <-
    trips %>%
    sf::st_drop_geometry(.) %>%
    data.table::setDT(.) %>%
    .[, date := lubridate::date(lubridate::as_datetime(start_local_time))] %>%
    # adds the date of local datetime of trip
    .[, .(
      n_trips = .N,
      n_trips_today = sum(date == Sys.Date()), # compares date of local datetime of trip to date of local time of dashboard user
      n_active_days = data.table::uniqueN(date),
      first_trip_datetime = min(start_fmt_time), # in UTC
      last_trip_datetime = max(start_fmt_time), # in UTC
      first_trip_local_datetime = format(min(lubridate::as_datetime(start_local_time)), usetz = FALSE),
      last_trip_local_datetime = format(max(lubridate::as_datetime(end_local_time)), usetz = FALSE)
    ), by = user_id] %>%
    .[, n_days := round(as.numeric(difftime(last_trip_datetime, first_trip_datetime, units = "days")), 1)]

  merge(participants, summ_trips, by = "user_id", all.x = TRUE)
}

#' Create a summary of server calls in data.table format.
#'
#' @param participants the output from `tidy_participants()`.
#' @param cons the connection to mongodb.
#'
#' @return a data.table.
#' @export
summarise_server_calls <- function(participants, cons) {
  # Get all of the relevant server calls
  usercache_get_summ <- query_usercache_get_summ(cons)
  usercache_put_summ <- query_usercache_put_summ(cons)
  diary_summ <- query_diary_summ(cons)

  # Get the column names that are in participants before merging.
  particpt_cols <- names(participants)


  # Merge each nonempty summary table with participants
  # Current columns to be added to participants: (first/last)_(get/put/diary)_call
  message("merging server call summaries with participants")
  merged <- FALSE
  if (nrow(usercache_get_summ > 0)) {
    participants <- merge(participants, usercache_get_summ, by = "user_id", all.x = TRUE)
    merged <- TRUE
  }
  if (nrow(usercache_put_summ > 0)) {
    participants <- merge(participants, usercache_put_summ, by = "user_id", all.x = TRUE)
    merged <- TRUE
  }
  if (nrow(diary_summ) > 0) {
    participants <- merge(participants, diary_summ, by = "user_id", all.x = TRUE)
    merged <- TRUE
  }

  message(ifelse(merged, "Finished merging server calls", "No server calls to merge"))

  all_cols <- names(participants)
  subset_cols <- all_cols[!all_cols %in% particpt_cols] # get the columns not in the original participants columns. a[!a %in% b] = "a without b"

  # Convert all server call time stamps to datetime by applying as_datetime to each server call column
  return(
    participants[, (subset_cols) := lapply(.SD, function(x) lubridate::as_datetime(x)), .SDcols = subset_cols]
  )
}

#' Tidy the 'cleaned locations' data.frame into a tibble.
#'
#' @param cleaned_locations a data.table output from `query_cleaned_trips()`.
#'
#' @return a tibble.
#' @export
tidy_cleaned_locations <- function(cleaned_locations) {
  message("Finished query, about to clean locations")

  tidied_locations <-
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

#' Tidy the 'server calls' data.frame into a tibble.
#'
#' @param server_calls a data.table output from `query_server_calls()`.
#'
#' @return a tibble.
#' @export
tidy_server_calls <- function(server_calls) {
  message("Finished query, about to tidy server calls")
  tidied_server_calls <-
    # flatten out names and select specific columns
    server_calls %>%
    setnames(gsub("data.", "", names(.))) %>%
    janitor::clean_names() %>%
    dplyr::select(user_id, name, ts, reading) %>%
    # convert datetime
    dplyr::mutate(
      fmt_time_utc = lubridate::as_datetime(ts)
    )
  message("Finished tidying server calls")
  return(tidied_server_calls)
}


#' Tidy together the 'tidied trips' and 'tidied locations' to generate trajectories within a trip
#'
#' @param tidied_trips a data.frame from `tidy_cleaned_trips()`.
#' @param tidied_locations a tibble from `tidy_cleaned_locations()`.
#' @param project_crs a EPSG code. Default as 4326.
#'
#' @return a data.frame of class sf.
#' @export
generate_trajectories <- function(tidied_trips, tidied_locations, project_crs = 4326) {
  message("About to generate trajectories")

  # drop trip geometries (just a line between OD) and set trips as data.table
  tidied_trips <- tidied_trips %>% sf::st_drop_geometry()
  data.table::setDT(tidied_trips)
  # set locations as data.table and duplicate location time to set an "interval"
  data.table::setDT(tidied_locations)[, fmt_time_utc_end := fmt_time_utc]
  # join locations to trips according to time interval overlap (all done in UTC time for consistency)
  data.table::setkey(tidied_locations, user_id, fmt_time_utc, fmt_time_utc_end)
  data.table::setkey(tidied_trips, user_id, start_fmt_time, end_fmt_time)
  joined_trips_and_locs <- data.table::foverlaps(tidied_locations, tidied_trips, nomatch = NULL)

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
  function(.data, cols, tz = "Australia/Sydney") {
    stopifnot(data.table::is.data.table(.data))
    .data[, `:=`(c(cols), lapply(.SD, function(.x) {
      lubridate::as_datetime(.x, tz = tz)
    })), .SDcols = cols]
  }
