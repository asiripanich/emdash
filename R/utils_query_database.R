#' Query functions
#'
#' @param cons a list of connections created with [connect_stage_collections()].
#' @param dates a date range, a character vector of length two.
#'  Dates must be in this format "yyyy-mm-dd".
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
query_cleaned_locations_by_timestamp <- function(cons, dates) {
  # Convert the dates to timestamps
  time_stamps <- as.numeric(as.POSIXct(dates))
  time_stamp_string <- sprintf('{\"$gte\": %s, \"$lte\": %s}', time_stamps[1], time_stamps[2])

  qstring <- sprintf(
    '{\"metadata.key\": \"analysis/recreated_location\", \"data.ts\": %s}', time_stamp_string
  )

  cons$Stage_analysis_timeseries$find(qstring) %>%
    as.data.table() %>%
    normalise_uuid()
}

#' @param sample_size number of location documents to be queried.
#' @rdname query
#' @export
#' @returns `downsample_cleaned_locations_by_timestamp` returns a dataframe containing
#' `sample_size` location documents within the date range.
downsample_cleaned_locations_by_timestamp <- function(cons, dates, sample_size) {

  # Convert the dates to timestamps
  time_stamps <- as.numeric(as.POSIXct(dates))
  time_stamp_string <- sprintf('{\"$gte\": %s, \"$lte\": %s}', time_stamps[1], time_stamps[2])

  match_string <- sprintf(
    '{\"$match\": {\"metadata.key\": \"analysis/recreated_location\", \"data.ts\": %s}}', time_stamp_string
  )

  # Take a sample of size `sample_size` after matching. Then remove the "_id" field.
  sample_string <- sprintf('{\"$sample\": {\"size\": %s}}', sample_size)
  project_string <-
    '{ \"$project\":
      {\"_id\": 0,
      \"user_id\": 1,
      \"metadata\": 1,
      \"data\": 1 }
    }'
  qstring <- sprintf(
    "[\n %s, \n %s, \n %s \n]", match_string, sample_string, project_string
  )

  cons$Stage_analysis_timeseries$aggregate(qstring) %>%
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

#' Finds the first and last get calls for each user.
#' @rdname query
#' @export
query_usercache_get_summ <- function(cons) {
  cons$Stage_timeseries$aggregate(
    '[
      { "$match": {"metadata.key": "stats/server_api_time", "data.name": "POST_/usercache/get"}},
      { "$group":
          {
            "_id": "$user_id",
            "first_get_call": { "$min": "$data.ts" },
            "last_get_call": {"$max": "$data.ts" }
          }
      },
      { "$project": {
            "user_id": "$_id",
            "_id": 0,
            "first_get_call": 1,
            "last_get_call": 1}
      }
    ]'
  ) %>%
    as.data.table() %>%
    normalise_uuid()
}

#' @rdname query
#' @return `query_usercache_put_summ()` returns the first and last
#'  put calls for each user.
#' @export
query_usercache_put_summ <- function(cons) {
  cons$Stage_timeseries$aggregate(
    '[
      { "$match": {"metadata.key": "stats/server_api_time", "data.name": "POST_/usercache/put"}},
      { "$group":
          {
            "_id": "$user_id",
            "first_put_call": { "$min": "$data.ts" },
            "last_put_call": {"$max": "$data.ts" }
          }
      },
      { "$project": {
            "user_id": "$_id",
            "_id": 0,
            "first_put_call": 1,
            "last_put_call": 1}
      }
    ]'
  ) %>%
    as.data.table() %>%
    normalise_uuid() %>%
    return()
}

#' Finds the first and last diary calls for each user.
#' @rdname query
#' @export
query_diary_summ <- function(cons) {
  cons$Stage_timeseries$aggregate(
    '[
      { "$match": {"metadata.key": "stats/server_api_time", "data.name": "POST_/pipeline/get_complete_ts"}},
      { "$group":
          {
            "_id": "$user_id",
            "first_diary_call": { "$min": "$data.ts" },
            "last_diary_call": {"$max": "$data.ts" }
          }
      },
      { "$project": {
            "user_id": "$_id",
            "_id": 0,
            "first_diary_call": 1,
            "last_diary_call": 1}
      }
    ]'
  ) %>%
    as.data.table() %>%
    normalise_uuid()
}

#' Finds the first and last get calls for each user.
#' @rdname query
#' @export
query_usercache_get_summ <- function(cons) {
  cons$Stage_timeseries$aggregate(
    '[
      { "$match": {"metadata.key": "stats/server_api_time", "data.name": "POST_/usercache/get"}},
      { "$group":
          {
            "_id": "$user_id",
            "first_get_call": { "$min": "$data.ts" },
            "last_get_call": {"$max": "$data.ts" }
          }
      },
      { "$project": {
            "user_id": "$_id",
            "_id": 0,
            "first_get_call": 1,
            "last_get_call": 1}
      }
    ]'
  ) %>%
    as.data.table() %>%
    normalise_uuid() %>%
    return()
}

#' Finds the first and last put calls for each user.
#' @rdname query
#' @export
query_usercache_put_summ <- function(cons) {
  cons$Stage_timeseries$aggregate(
    '[
      { "$match": {"metadata.key": "stats/server_api_time", "data.name": "POST_/usercache/put"}},
      { "$group":
          {
            "_id": "$user_id",
            "first_put_call": { "$min": "$data.ts" },
            "last_put_call": {"$max": "$data.ts" }
          }
      },
      { "$project": {
            "user_id": "$_id",
            "_id": 0,
            "first_put_call": 1,
            "last_put_call": 1}
      }
    ]'
  ) %>%
    as.data.table() %>%
    normalise_uuid() %>%
    return()
}

#' Finds the first and last diary calls for each user.
#' @rdname query
#' @export
query_diary_summ <- function(cons) {
  cons$Stage_timeseries$aggregate(
    '[
      { "$match": {"metadata.key": "stats/server_api_time", "data.name": "POST_/pipeline/get_complete_ts"}},
      { "$group":
          {
            "_id": "$user_id",
            "first_diary_call": { "$min": "$data.ts" },
            "last_diary_call": {"$max": "$data.ts" }
          }
      },
      { "$project": {
            "user_id": "$_id",
            "_id": 0,
            "first_diary_call": 1,
            "last_diary_call": 1}
      }
    ]'
  ) %>%
    as.data.table() %>%
    normalise_uuid() %>%
    return()
}

#' @rdname query
#' @export
query_cleaned_trips <- function(cons) {
  cons$Stage_analysis_timeseries$find('{"metadata.key": "analysis/confirmed_trip"}') %>%
    as.data.table() %>%
    normalise_uuid() %>%
    data.table::setorder(data.end_fmt_time)
}

#' @param n number of trips used by `query_most_recent_n_trip_docs()`
#' @rdname query
#' @export
#' @returns `query_most_recent_n_trip_docs()` yields trips data for the most recent n
#' trips, based on data.end_ts
query_most_recent_n_trip_docs <- function(cons, n) {
  cons$Stage_analysis_timeseries$find('{"metadata.key": "analysis/confirmed_trip"}',
    sort = '{"data.end_ts" : -1}', limit = n
  ) %>%
    as.data.table() %>%
    normalise_uuid() %>%
    data.table::setorder(data.end_fmt_time)
}

#' @rdname query
#' @export
#' @returns `query_confirmed_trips_by_timestamp()` trips data
#'  between the start timestamp of the first date
#'  and the start timestamp of the second date.
query_confirmed_trips_by_timestamp <- function(cons, dates) {
  # Convert the dates to timestamps
  time_stamps <- as.numeric(as.POSIXct(dates))
  time_stamp_string <- sprintf('{\"$gte\": %s, \"$lte\": %s}', time_stamps[1], time_stamps[2])

  qstring <- sprintf(
    '{\"metadata.key\": \"analysis/confirmed_trip\", \"data.end_ts\": %s}', time_stamp_string
  )

  # The query string should have this format
  # {"metadata.key": "analysis/confirmed_trip", "data.end_ts":{"$gte": 1437544800,"$lte": 1451286000}}

  cons$Stage_analysis_timeseries$find(qstring)
}

#' Finds the maximum trip end timestamp.
#' @rdname query
#' @export
query_max_trip_timestamp <- function(cons) {
  cons$Stage_analysis_timeseries$aggregate(
    '[
      { "$match": {"metadata.key": "analysis/confirmed_trip"}},
      { "$group":
          {
            "_id": {},
            "max_ts": { "$max": "$data.end_ts" }
          }
      }]'
  ) %>%
    unlist() %>%
    unname()
}

#' Finds the minimum trip start timestamp.
#' @rdname query
#' @export
query_min_trip_timestamp <- function(cons) {
  cons$Stage_analysis_timeseries$aggregate(
    '[
      { "$match": {"metadata.key": "analysis/confirmed_trip"}},
      { "$group":
          {
            "_id": {},
            "min_ts": { "$min": "$data.start_ts" }
          }
      }]'
  ) %>%
    unlist() %>%
    unname()
}

#' @returns Counts the total number of trips in the database
count_total_trips <- function(cons) {

  # for each user_id, count the number of documents associated with it
  cons$Stage_analysis_timeseries$aggregate('[
                    {"$match": {"metadata.key": "analysis/confirmed_trip"}},
                    {"$group": {"_id":"$user_id","n_trips":{"$sum":1}}}
                    ]') %>%
    as.data.table() %>%
    setnames("_id", "user_id") %>%
    normalise_uuid()
}

# Gets date and mode related trip information for all trips
query_trip_dates <- function(cons, confirmed_user_input_column) {
  small_trip_query <- cons$Stage_analysis_timeseries$find(
    query = '{"metadata.key": "analysis/confirmed_trip"}',
    fields = sprintf('{"data.start_local_dt":true,
              "data.start_fmt_time":true,
              "data.end_local_dt":true,
              "data.end_fmt_time":true,
              "%s":true,
              "user_id":true,
              "_id":false}', confirmed_user_input_column)
  )
  return(small_trip_query)
}

#' Get manual survey responses
#' 
#' @description 
#' This function returns the manual survey response objects stored in
#' the 'Stage_timeseries' collection. These are Enketo responses that
#' emTripLog and its Forks applications stored.
#' 
#' @cons an e-mission MongoDB connection object.
#' 
#' @return a data.table object
#' @export
query_manual_survey_response <- function(cons) {
  cons$Stage_timeseries$find(
    query = '{"metadata.key": "manual/survey_response"}'
  ) %>% 
  as.data.table()
}

#' @rdname query_manual_survey_response
#' @export 
query_time_use_response <- function(cons) {
  cons$Stage_timeseries$find(
    query = '{"metadata.key" : "manual/survey_response", "data.name" : "TimeUseSurvey"}'
  ) %>% 
  as.data.table()
}

#' @rdname query_manual_survey_response
#' @export 
query_trip_confirm_response <- function(cons) {
  cons$Stage_timeseries$find(
    query = '{"metadata.key" : "manual/survey_response", "data.name" : "TripConfirmSurvey"}'
  ) %>% 
  as.data.table()
}

#' @rdname query
#' @export
#' @return `get_n_trips_in_query()` returns the number of cleaned
#'  trip documents in between two dates
#' @export
get_n_trips_in_query <- function(cons, dates) {
  time_stamps <- as.numeric(as.POSIXct(dates))
  message("get_n_trips_in_query: The time stamps for that date range are:")
  message(paste0(time_stamps[1], ", ", time_stamps[2]))
  lower_stamp_string <- paste0('{\"$gte\": ', time_stamps[1], ",")
  upper_stamp_string <- paste0('\"$lte\": ', time_stamps[2], "}")

  # Match by the time stamps of the dates
  match_string <- paste0(
    '{\"$match\":{\"metadata.key\": \"analysis/confirmed_trip\", ',
    '\"data.end_ts\":', lower_stamp_string, upper_stamp_string,
    "}}"
  )

  # Group by all user_ids
  group_string <- '{\"$group\": {\"_id\": {},\"n_trips\":{\"$sum\":1}}}'

  qstring <- paste0(
    "[\n", match_string, ",",
    group_string, "\n]"
  )

  # for each user_id, count the number of documents associated with it
  cons$Stage_analysis_timeseries$aggregate(qstring) %>% .$n_trips
}

#' @rdname query
#' @export
#' @description Returns the number of cleaned locations documents in between two dates
get_n_locations_in_query <- function(cons, dates) {
  time_stamps <- as.numeric(as.POSIXct(dates))
  lower_stamp_string <- paste0('{\"$gte\": ', time_stamps[1], ",")
  upper_stamp_string <- paste0('\"$lte\": ', time_stamps[2], "}")

  # Match by the time stamps of the dates
  match_string <- paste0(
    '{\"$match\":{\"metadata.key\": \"analysis/recreated_location\", ',
    '\"data.ts\":', lower_stamp_string, upper_stamp_string,
    "}}"
  )

  # Group by all user_ids
  group_string <- '{\"$group\": {\"_id\": {},\"n_locations\":{\"$sum\":1}}}'

  qstring <- paste0(
    "[\n", match_string, ",",
    group_string, "\n]"
  )

  # for each user_id, count the number of documents associated with it
  cons$Stage_analysis_timeseries$aggregate(qstring) %>% .$n_locations
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
