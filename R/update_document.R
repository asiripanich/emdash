
# Write function to add a field to a given document.

# Updates a participant document with the row 'df_row'
db_update_participants <- function(cons,collection_name,df_row){
  
  # User id is the only identifier needed since participants has one entry per user id
  # BUT how should participants be handled in the database, since it is actually scattered between
  # Stage_timeseries stats/server_api_time (server calls), 
  # stage profiles, stage uuids, and stage analysis time series (trips) 
  
  user_id_string <- sprintf('{\"user_id\": \"%s\"}', df_row$user_id)
  
  set_string <- sprintf('{\"$set":{\"recommendation\": \"%s\"}}',df_row$recommendation)
  
  cons[[collection_name]]$update(user_id_string,set_string)
}

# db_update_recomm <- function(cons, collection_name, df_row) {
#   # The format is seen below:
#   # db$collection$update('{"name":"jerry"}', '{"$set":{"age": 31}}')
#   # Example user_id_string: {"user_id": "Extra_User", "bikeLabel": "WestTower"}
#   
#   user_id_string <- paste0('{\"user_id\": \"', df_row$user_id, '\", ', '\"bikeLabel\": \"', df_row$bikeLabel, '\"}') # %>% cat
#   new_status <- isTRUE(df_row$status %>% as.character() %>% as.logical())
#   logic_string <- ifelse(new_status, "true", "false") # if status is TRUE, output 'true'
#   
#   set_string <- paste0('{\"$set":{\"status\":', logic_string, "}}") # %>% cat
#   
#   cons[[collection_name]]$update(user_id_string, set_string)
# }
# 
# 
# user_id <- paste(25)
# recommendation <- 'check always permission'
# updated_bike <- data.frame(user_id,recommendation)
# coll <- "Checkinout"
# cons <- connect_stage_collections()
# 
# 
# db_update_participants(cons,coll, updated_bike)


