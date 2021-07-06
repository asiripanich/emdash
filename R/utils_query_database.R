#' Query functions
#'
#' @param cons a list of connections created with [connect_stage_collections()].
#'
#' @return a data.frame/data.table.
#' @name query
#' @export

#' @rdname query
#' @export
query_cleaned_locations <- function(cons) {
  # to implement later - query only for locations between a range of timestamps (as specified in map filter)
  # cons$Stage_analysis_timeseries$find('{"metadata.key": "analysis/recreated_location", "data.ts": {"$lte": range_end_ts, "$gte": range_start_ts}}') %>%
  cons$Stage_analysis_timeseries$find('{"metadata.key": "analysis/recreated_location"}') %>%
    as.data.table() %>%
    normalise_uuid()
}

#' @rdname query
#' @export
query_server_calls <- function(cons) {
  # to implement later - query only for the API call patterns that are relevant
  # the problem is that if there aren't any entries (might happen if the user
  # has just signed up and not taken any trips), then numerous downstream calls fail
  # Fixes for downstream calls are in a patch in the PR
  # cons$Stage_timeseries$find('{"metadata.key": "stats/server_api_time", "data.name": {"$regex": "/usercache|get_complete_ts/", "$options": ""}}') %>%
  cons$Stage_timeseries$find('{"metadata.key": "stats/server_api_time"}') %>%
    as.data.table() %>%
    normalise_uuid()
}

#' @rdname query
#' @export
query_cleaned_trips <- function(cons) {
  cons$Stage_analysis_timeseries$find('{"metadata.key": "analysis/confirmed_trip"}') %>%
    as.data.table() %>%
    normalise_uuid() %>%
    data.table::setorder(data.end_fmt_time)
}

#' @rdname query
#' @export
query_checkinout <- function(cons){
  # cons <- connect_stage_collections(url = getOption('emdash.mongo_url')) 
  cons$Checkinout$find('{}') %>%
    as.data.table() %>%
    normalise_uuid()
}

#' @rdname query
#' @export
query_supplementary <- function(cons, name) {
  new_table <- cons[[name]]$find('{}') %>% as.data.table()
  
  if ('ts' %in% colnames(new_table)){
    date_time_name <- ifelse(name == 'Checkinout', 'Checkout_time','Datetime')
    
    # Convert the timestamp to a character and append the timezone, 
    # since we want it in the system timezone but DT::datatable displays POSIXct objects in UTC.
    temp_datetime <- lubridate::as_datetime(new_table$ts, tz = Sys.timezone())
    tzone <- attr(temp_datetime,"tzone")
    new_table[[date_time_name]] <- paste(as.character(temp_datetime),tzone)
  }
  return(new_table)

  # normalise_uuid() is done after this is called 
  # within mod_load_data because Tier_Sys has no user_id column in the test data
}

#' @rdname query
#' @export
query_cleaned_place <- function(cons) {
  cons$Stage_analysis_timeseries$find('{"metadata.key": "analysis/cleaned_place"}') %>%
    as.data.table() %>%
    normalise_uuid()
}

#' @rdname query
#' @export
query_cleaned_section <- function(cons) {
  cons$Stage_analysis_timeseries$find('{"metadata.key": "analysis/cleaned_section"}') %>%
    as.data.table() %>%
    normalise_uuid()
}

#' @rdname query
#' @export
query_raw_trips <- function(cons) {
  cons$Stage_analysis_timeseries$find('{"metadata.key": "segmentation/raw_trip"}') %>%
    as.data.table() %>%
    normalise_uuid() %>%
    data.table::setorder(data.end_fmt_time)
}

#' @rdname query
#' @export
query_stage_uuids <- function(cons) {
  cons$Stage_uuids$find() %>%
    as.data.table(.) %>%
    normalise_uuid(., keep_uuid = FALSE)
}

#' @rdname query
#' @export
query_stage_profiles <- function(cons) {
  cons$Stage_Profiles$find() %>%
    as.data.table() %>%
    normalise_uuid(., keep_uuid = FALSE)
}

#' Normalise UUID
#'
#' @param .data a data.frame.
#' @param keep_uuid logical.
#'
#' @return a data.frame
#' @export
normalise_uuid <- function(.data, keep_uuid = FALSE) {
  if (nrow(.data) == 0) {
    return(.data)
  }
  # return(.data)
  if (!is.data.table(.data)) {
    setDT(.data)
  }
  if ("uuid" %in% names(.data)) {
    .data[, user_id := sapply(uuid, function(.x) paste0(unlist(.x), collapse = ""))]
    if (!keep_uuid) {
      .data[, uuid := NULL]
    }
  } else {
    .data[, user_id := sapply(user_id, function(.x) paste0(unlist(.x), collapse = ""))]
  }
  .data
}

#' Anonymize user_id field
#'
#' @param .data a data.frame object
#'
#' @return .data with anonymized user_id
#' @export
anonymize_uuid <- function(.data) {
  stopifnot(is.data.frame(.data))
  if (!"user_id" %in% names(.data)) {
    stop("There is no `user_id` field in `.data`.")
  }
  unique_uuid <- unique(.data$user_id)
  anon_uuid <- paste0("user_", sample(length(unique_uuid)))
  names(anon_uuid) <- unique_uuid
  .data$user_id <- anon_uuid[.data$user_id]
  .data
}

anonymize_uuid_if_required <- function(.data, flag = getOption("emdash.anon_locations")) {
  checkmate::assert_flag(flag, null.ok = FALSE)
  if (flag) {
    message("Anonymize trajectories")
    return(anonymize_uuid(.data))
  }
  .data
}

#' Convert character columns to datetime columns
#'
#' @param .data a data.frame.
#' @param cols columns to convert to datetime columns.
#' @param tz time zone. Default as "Australia/Sydney".
#'
#' @return .data
#' @export
convert_datetime_string_to_datetime <- function(.data, cols, tz = "Australia/Sydney") {
  stopifnot(data.table::is.data.table(.data))
  .data[, c(cols) := lapply(.SD, function(.x) {
    lubridate::as_datetime(.x, tz = tz)
  }), .SDcols = cols]
}
