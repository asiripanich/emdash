# participants after summarise_trips_without_trips matches participants after summarise_trips

    Code
      participants_using_trips
    Output
                                  user_id      user_email           update_ts client
      1: 192c82d3c1da4a679edaceb81693fce1        testuser 2020-12-17 23:17:50     NA
      2: d38b6b9e442a4b5baaabf7bf39018895 testanotheruser 2020-10-14 17:31:31     NA
         curr_platform n_trips n_trips_today n_active_days first_trip_datetime
      1:       android      50             0             8 2016-07-22 16:26:08
      2:           ios      NA            NA            NA                <NA>
          last_trip_datetime first_trip_local_datetime last_trip_local_datetime
      1: 2016-08-11 21:05:09       2016-07-22 09:26:08      2016-08-11 14:45:45
      2:                <NA>                      <NA>                     <NA>
         n_days
      1:   20.2
      2:     NA

---

    Code
      participants_using_db_trip_summary
    Output
                                  user_id      user_email           update_ts client
      1: 192c82d3c1da4a679edaceb81693fce1        testuser 2020-12-17 23:17:50     NA
      2: d38b6b9e442a4b5baaabf7bf39018895 testanotheruser 2020-10-14 17:31:31     NA
         curr_platform n_trips n_trips_today n_active_days first_trip_datetime
      1:       android      50             0             8 2016-07-22 16:26:08
      2:           ios      NA            NA            NA                <NA>
          last_trip_datetime first_trip_local_datetime last_trip_local_datetime
      1: 2016-08-11 21:05:09       2016-07-22 09:26:08      2016-08-11 14:45:45
      2:                <NA>                      <NA>                     <NA>
         n_days unconfirmed
      1:   20.2           0
      2:     NA           0

