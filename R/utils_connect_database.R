#' Connect to the e-mission's MongoDB database
#'
#' @param collections collections to connect to, by default this try to connect to
#' all the collections in [COLLECTIONS].
#' @param db database name. Default as 'Stage_database'.
#' @param url address of the emisison mongodb server in mongo
#'   connection string [URI format](https://docs.mongodb.com/manual/reference/connection-string/).
#'   The default is `getOption('emdash.mongo_url')`
#'
#' @return a list of mongolite connection objects.
#' @export
connect_stage_collections <- function(db = "Stage_database", collections = COLLECTIONS, url = getOption("emdash.mongo_url")) {
  cons <- lapply(collections, function(x) create_connection(db = db, collection = x, url = url))
  names(cons) <- collections
  cons
}

create_connection <- function(db, collection, url) {
  stopifnot(collection %in% COLLECTIONS)
  mongolite::mongo(
    db = db,
    collection = collection,
    url = url
  )
}

#' Collections in an e-mission MongoDB database
#'
#' @description
#'
#' There are 7 collections that exist in a e-mission data.
#'
#' * Stage_Profiles
#' * Stage_analysis_timeseries
#' * Stage_pipeline_state
#' * Stage_timeseries
#' * Stage_timeseries_error
#' * Stage_usercache
#' * Stage_uuids
#' @export
"COLLECTIONS"

COLLECTIONS <-
  c(
    "Stage_Profiles",
    "Stage_analysis_timeseries",
    "Stage_pipeline_state",
    "Stage_timeseries",
    "Stage_timeseries_error",
    "Stage_usercache",
    "Stage_uuids"
  )
