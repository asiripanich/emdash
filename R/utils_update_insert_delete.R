
#' Inserts a row as a document into the specified collection
db_insert <- function(cons,collection_name,df) {
  cons[[collection_name]]$insert(df)
}

#' Updates a document with the row 'df_row'
db_update <- function(cons,collection_name,df_row){
  # The format is seen below:
  # db$collection$update('{"name":"jerry"}', '{"$set":{"age": 31}}')
  # Example user_id_string: {"user_id": "Extra_User", "bikeLabel": "WestTower"}
  
  user_id_string <- paste0('{\"user_id\": \"',df_row$user_id,'\", ','\"bikeLabel\": \"',df_row$bikeLabel, '\"}')  #%>% cat
  new_status <- isTRUE(df_row$status %>% as.character() %>% as.logical())
  logic_string <- ifelse(new_status,'true','false')  # if status is TRUE, output 'true'
  
  set_string <- paste0('{\"$set":{\"status\":',logic_string,'}}') # %>% cat
  
  cons$Checkinout$update(user_id_string,set_string)
}


#' Deletes a document from Checkinout corresponding to df_row
db_delete <- function(cons,collection_name,df_row){

  if (df_row$user_id == '') {
    # In case the entry has no user id
    user_id_string <- paste0('{\"user_id\": {\"$exists\" : "false" }, ','\"bikeLabel\": \"',df_row$bikeLabel, '\"}') 
    cons$Checkinout$remove(user_id_string, just_one = TRUE)
  } else {
    user_id_string <- paste0('{\"user_id\": \"',df_row$user_id,'\", ','\"bikeLabel\": \"',df_row$bikeLabel, '\"}') 
    cons$Checkinout$remove(user_id_string, just_one = TRUE)
  }
}

# cons <- connect_stage_collections(url = getOption('emdash.mongo_url'))
#t <- cons$Checkinout$find()
## Example
# Setting up extra entries to delete or edit.
# for (j in 1:60){
#   user_id <- paste(j)
#   status <- FALSE
#   bikeLabel <- ifelse(j %% 2, 'WestTower', 'EastTower')
#   ts <- Sys.time() %>% as.POSIXct() %>% as.numeric() + j
#   new_bike <- data.frame(user_id,status,bikeLabel,ts)
#   db_insert(cons,"Checkinout",new_bike)
# }

# user_id <- 'Extra_User'
# status <- T
# bikeLabel <- 'WestTower'
# updated_bike <- data.frame(user_id,status,bikeLabel)

# cons$Checkinout$update('{"user_id": "Extra_User"}','{"$set":{"status": false}}')
# 
# db_update(cons,"Checkinout",updated_bike)

# as.POSIXct(x, tz = "", ...)
