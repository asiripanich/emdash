#' Query functions
#'
#' @param cons a list of connections created with [connect_stage_collections()].
#'
#' @return a data.frame/data.table.
#' @name query
#' @export

#' @rdname query
#' @export
query_cleaned_trips <- function(cons) {
  cons$Stage_analysis_timeseries$find('{"metadata.key": "analysis/cleaned_trip"}') %>%
    as.data.table() %>%
    normalise_uuid() %>%
    data.table::setorder(data.end_fmt_time)
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
query_trip_ends <- function(cons) {
  .data <-
    cons$Stage_timeseries$find('{"metadata.key": "manual/confirm_survey"}') %>%
    as.data.table() %>%
    .[, data.survey_result := lapply(data.survey_result, function(.x) {
      list(xml2::read_xml(.x) %>% xml2::as_list() %>% unlist())
    })]
  
  tripend_survey_responses <-
    .data$data.survey_result %>%
    purrr::flatten(.) %>%
    purrr::map_dfr(., ~ {
      .x %>%
        matrix(byrow = T, nrow = 1, dimnames = list(c(), names(.))) %>%
        as.data.table()
    })
  
  cbind(tripend_survey_responses, .data) %>%
    data.table::setcolorder(c("user_id", "data.timestamp")) %>%
    normalise_uuid(.)
  
}

#' @rdname query
#' @export
query_user_profile_survey <- function(cons) {
  cons$Stage_timeseries$find('{"metadata.key": "manual/user_profile_survey"}') %>%
    as.data.table() %>%
    normalise_uuid() %>%
    .[, data.datetime := as_datetime(data.timestamp, tz = "Australia/Sydney")]
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
    normalise_uuid(., keep_uuid = FALSE) %>%
    convert_datetime_string_to_datetime(cols = c("update_ts"))
}

#' Normalise UUID
#'
#' @param .data a data.frame.
#' @param keep_uuid logical.
#'
#' @return a data.frame
#' @export
normalise_uuid <- function(.data, keep_uuid = FALSE) {
  # return(.data)
  if (!is.data.table(.data)) {
    setDT(.data)
  }
  if ("uuid" %in% names(.data)) {
    .data[, user_id := sapply(uuid, function(.x) paste0(unlist(.x), collapse = ""))]
    if (!keep_uuid)  {
      .data[, uuid := NULL]
    }
  } else {
    .data[, user_id := sapply(user_id, function(.x) paste0(unlist(.x), collapse = ""))]
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
  .data[, c(cols) := lapply(.SD, function(.x) {lubridate::as_datetime(.x, tz = tz)}), .SDcols = cols]
}

