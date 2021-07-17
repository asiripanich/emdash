
# Define criteria for inactive/not recent
last_trip_gt_value <- 3
last_communication_gt_value <- 2 
last_data_upload_gt_value <- 2

participants <- read.csv('C:/Users/mallen2/participants.csv') %>% as.data.table()

# cols to represent as dates:
check_validity_columns <- c('Last.trip.date', 'last.app.communication','last.data.upload')

# Pick a 'today'
reference_date <- lubridate::as_datetime("2021-02-02") 
                  # participants$last.app.communication %>% lubridate::as_datetime() %>% max(na.rm= TRUE)
                  # "2021-02-01 06:59:18 UTC"


participants <- participants[, (check_validity_columns) := 
                               lapply(.SD, function(x) lubridate::as_datetime(x)), 
                             .SDcols = check_validity_columns]

gt_or_na <- function(x, lower_bound) {
  x > lower_bound | is.na(x)
}

# cols to check to determine active/inactive and status
check_validity_columns <- c('Last.trip.date', 'last.app.communication','last.data.upload')

elapsed_time_cols <- c('last_trip_elapsed_time', 
                       'last_communication_elapsed_time', 
                       'last_data_upload_elapsed_time')

inactive_subset_columns <- c('android.iOS','android.iOS.version',elapsed_time_cols)

status_levels <- c('check always permission or app force kill',
                   'check always permission',
                   'check token',
                   'check aggressive power savings',
                   'unknown')

inactive_participants <-
  
  # generate new columns to store the length of time since each relevant communication
  participants[, (elapsed_time_cols) := 
                 lapply(.SD, function(x) difftime(reference_date,x,units = "days")),
               .SDcols = check_validity_columns] %>% 
  
  # look at only rows that are considered inactive_participants
  .[gt_or_na(last_trip_elapsed_time,last_trip_gt_value) | 
     gt_or_na(last_communication_elapsed_time,last_communication_gt_value) | 
     gt_or_na(last_data_upload_elapsed_time,last_data_upload_gt_value), ] %>% 
  
  # Subset the columns for the features we want to look at
  .[, ..inactive_subset_columns] %>%

  # Add a status label column
  # Make sure each label is consistent with one of the status levels
  .[, status := fcase(
    
    # Apple phone, no app communication, no data upload: 
    # Most likely have the “always” permission turned off and have “closed”/force-killed the app
    (android.iOS == 'ios' & 
       gt_or_na(last_communication_elapsed_time,last_communication_gt_value) & 
       gt_or_na(last_data_upload_elapsed_time,last_data_upload_gt_value)
     ), 
    factor(status_levels[1], levels = status_levels),
    
    # Android 11, recent app communication, but no data upload: 
    # Most likely have "always" permission turned off
    ( android.iOS == 'android' & (android.iOS.version >= 11 | android.iOS.version < 12) &
        last_communication_elapsed_time < last_communication_gt_value &
        gt_or_na(last_data_upload_elapsed_time, last_data_upload_gt_value)
    ), 
    factor(status_levels[2], levels = status_levels),
    
    # Both OSes, no recent app communication: token missing or not configured properly?
    (gt_or_na(last_communication_elapsed_time,last_communication_gt_value) 
    ),
    factor(status_levels[3], levels = status_levels),
        
    # Samsung phone acting weirdly
    # Android < 11, recent communication, but no data: likely aggressive power savings https://dontkillmyapp.com/
    (android.iOS == 'android' & android.iOS.version < 11 & last_communication_elapsed_time < last_communication_gt_value &
       gt_or_na(last_data_upload_elapsed_time,last_data_upload_gt_value)
     ),
    factor(status_levels[4], levels = status_levels),
    
    default = factor('unknown', levels = status_levels)
  ) ]





# Rules

# Apple phone, no app communication, no data upload: Most likely have the “always” permission turned off and have “closed”/force-killed the app
# Android 11, recent app communication, but no data upload: Most likely have "always" permission turned off
# Both OSes, no recent app communication: token missing or not configured properly?
# Samsung phone acting weirdly
# Android < 11, communication, but no data: likely aggressive power savings https://dontkillmyapp.com/
  

# client_app_version -> smartphone app version
# client_os_version -> android/iOS version
# curr_platform -> android/iOS
# manufacturer

# 'make',
# 'model',
# 'OS',

