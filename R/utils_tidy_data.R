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

#' Tidy the 'cleaned trips' data.frame into a sf object.
#'
#' @param cleaned_trips a data.table output from `query_cleaned_trip()`.
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
      start_fmt_time = lubridate::as_datetime(start_fmt_time),
      end_fmt_time = lubridate::as_datetime(end_fmt_time)
    ) %>%
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
        "_lon",
        "_coordinates"
      )
    )) %>%
    dplyr::select(
      user_id,
      start_fmt_time,
      start_local_dt_timezone,
      end_fmt_time,
      end_local_dt_timezone,
      dplyr::everything()
    )
  message("Finished cleaning trips")
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
    .[, date := lubridate::date(start_fmt_time)] %>%
    .[, .(
      n_trips = .N, 
      n_trips_today = sum(date == Sys.Date()),
      n_active_days = data.table::uniqueN(date),
      first_trip_datetime = min(start_fmt_time),
      last_trip_datetime = max(start_fmt_time)
    ), by = user_id] %>%
    .[, n_days := as.numeric(difftime(last_trip_datetime, first_trip_datetime, units = "days"))]
  
  merge(participants, summ_trips, by = "user_id", all.x = TRUE)
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
