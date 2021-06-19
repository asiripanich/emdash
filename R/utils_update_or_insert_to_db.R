


db_insert <- function(cons,collection_name,df) {
  cons[[collection_name]]$insert(df)
}

# not as general at the moment
db_update <- function(cons,collection_name,df_row){
  # format
  # db$collection$update('{"name":"jerry"}', '{"$set":{"age": 31}}')
  # Example user_id_string: {"user_id": "Extra_User", "bikeLabel": "WestTower"}
  user_id_string <- paste0('{\"user_id\": \"',df_row$user_id,'\", ','\"bikeLabel\": \"',df_row$bikeLabel, '\"}')  %>% cat
  logic_string <- ifelse(df_row$status,'true','false')  # if status is TRUE, output 'true'
  
  set_string <- paste0('{\"$set":{\"status\":',logic_string,'}}') # %>% cat
  
  cons$Checkinout$update(user_id_string,set_string)
}

db_delete <- function(cons,collection_name,df_row){
  user_id_string <- paste0('{\"user_id\": \"',df_row$user_id,'\", ','\"bikeLabel\": \"',df_row$bikeLabel, '\"}') 
  cons$Checkinout$remove(user_id_string, just_one = TRUE)
}

# Example
# user_id <- '123Extra_User'
# status <- TRUE
# bikeLabel <- 'WestTower'
# new_bike <- data.frame(user_id,status,bikeLabel)
# 
# db_insert(cons,"Checkinout",new_bike)


# user_id <- 'Extra_User'
# status <- T
# bikeLabel <- 'WestTower'
# updated_bike <- data.frame(user_id,status,bikeLabel)

# cons$Checkinout$update('{"user_id": "Extra_User"}','{"$set":{"status": false}}')
# 
# db_update(cons,"Checkinout",updated_bike)
