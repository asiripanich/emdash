# participants after summarise_trips_without_trips matches participants after summarise_trips

    Code
      participants_using_trips
    Output
                                  user_id      user_email           update_ts client
      1: 192c82d3c1da4a679edaceb81693fce1        testuser 2020-12-17 23:17:50     NA
      2: d38b6b9e442a4b5baaabf7bf39018895 testanotheruser 2020-10-14 17:31:31     NA
         curr_platform n_trips n_trips_today n_active_days first_trip_datetime
      1:       android      50             0             8 2016-07-22 16:26:08
      2:           ios      22             0             4 2015-07-22 15:14:53
          last_trip_datetime first_trip_local_datetime last_trip_local_datetime
      1: 2016-08-11 21:05:09       2016-07-22 09:26:08      2016-08-11 14:45:45
      2: 2015-08-28 02:20:57       2015-07-22 08:14:53      2015-08-27 19:32:22
         n_days
      1:   20.2
      2:   36.5

---

    Code
      participants_using_db_trip_summary
    Output
                                  user_id      user_email           update_ts client
      1: 192c82d3c1da4a679edaceb81693fce1        testuser 2020-12-17 23:17:50     NA
      2: d38b6b9e442a4b5baaabf7bf39018895 testanotheruser 2020-10-14 17:31:31     NA
         curr_platform n_trips n_trips_today n_active_days first_trip_datetime
      1:       android      50             0             8 2016-07-22 16:26:08
      2:           ios      22             0             4 2015-07-22 15:14:53
          last_trip_datetime first_trip_local_datetime last_trip_local_datetime
      1: 2016-08-11 21:05:09       2016-07-22 09:26:08      2016-08-11 14:45:45
      2: 2015-08-28 02:20:57       2015-07-22 08:14:53      2015-08-27 19:32:22
         n_days unconfirmed
      1:   20.2           0
      2:   36.5          22

