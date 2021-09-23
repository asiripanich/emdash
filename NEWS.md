<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# emdash 1.6.0

- Update the test database to also include inferred labels. (thanks @shankari!)
- Update tests that had values depended on the previous test database.
- The `mod_mapview` server module now drops all list columns in its `data_sf` argument. This change is required to remove `inferred_label` (see asiripanich#69) from the map data which is a spatial feature object.
- Unify the default `datatable_options` in one place.
- `run_app()` now checks if `getOption("emdash.config_file")` returns anything, otherwise it uses the default config file, `inst/config-default.yml`, for the `config_file` argument.
- Data from specific users can be removed from the dashboard by specifying their ids or emails in a txt file. Use `remove_from_participants_col` and `remove_from_participants_file` file in the emdash configuration file.
- The dashboard no longer shows the bike checkin table (which is specific to the CanBikeCO study) and the polar bear table by default.
- Editable table feature: switched to object ID to identify which Checkinout entry to delete. It used to use user_id, but that did not delete database entries because it was not converted to BSON before querying.
- Use GitHub Actions to build emdash Docker image and push to Docker Hub


# emdash 1.5.0

- Add a test MongoDB database to the R CMD check that runs GitHub Actions.
- Fix the mismatch between the colors on the map and the legend. Unfortunately, this is a quick fix only, by going back to using the mapview's default palatte, which seems to be the only option that works. (#56)
- Refactor `run_app()`.
- Fix `summarise_trips_without_trips()`, the function was using the min and max dates for all trips, rather than the min and max for each user. (#54)
- Better document the options in the YAML config file.
- The color label of the map is now configurable from the config file by changing the `map_trajectory_colors_variable` field. By default this field is set to 'user_id'. To color label by travel mode change to `mode_confirm`.
- Fixes bad column names that can crash the dashboard on the first reload of the trip data. (#57)
- Add a boolean option for displaying `user_email` on the trip table. See `trips_table_merge_user_email` in the config file. (#57)


# emdash 1.4.1

- The plot is now synced all the tables. (#55)
- Correct a variable name. In config, `col_labels_for_participts` get renamed to `cols_to_remove_from_participts_table`.


# emdash 1.4.0

- Trip loading and location loading now depend on the number of (MongoDB) documents in the date range and the specified max documents per trip found in config-default.yml. See #49 for an explanation of this feature.
- Make supplement tables editable. This option can be configured using the configuration file of your dashboard.


# emdash 1.3.0

- Trips can be filtered using the date range selector from the sidebar. This only affects the "Trip Trend" plot in the Dashboard panel and the trips displayed on the Maps panel. By default, only trips from the last 30 days since the last recorded trips are selected. Also, the max number of days the user can select must not be more than the `max_windows_for_mod_load_trips` option in your config file.
- Fix minor issue with filtering trip table entries using the config.yml.


# emdash 1.2.6

- The map no longer shows detailed trajectories of the participants by default. See issues #29 and #40 related to this in problem.
- Add some addition fields to ignore

# emdash 1.2.5

- Added minor cosmetic changes to the dashboard (#24)
- YAML file made to configure options for the dashboard.
- Moved anon_locations and cols_to_remove_from_ (trips and map_popup) to the YAML
- Added `save_config_file()` for saving the default app config file.
- Added version number to the dashboard UI

# emdash 1.2.4

- Display user labels generically in the dashboard (#23, @shankari).

# emdash 1.2.3

- Updated the author list.

# emdash 1.2.2

- Added support for displaying the server communication status on the participant table.

# emdash 1.2.1

-  Hid the "source" column from the trip table and the map. 

# emdash 1.2.0

- Fixed [#15](https://github.com/asiripanich/emdash/issues/15).
- Fixed the issue with trips didn't show when loading the map without adjusting any of the filter options.

# emdash 1.1.0

## Major changes

- The map now displays trips' trajectories, `generate_trajectories()` is used to generate them. Warning: this is curently performed at loading - future versions could wait to do this until "show trajectories" is selected on the map view and after desired trip filters are applied
- New data columns for local timestamps, duration in min, distances in mi and km. Some of these and others are later hidden from view in tables/map filter/map popup.
- Add a column visibility button to the tables.

# emdash 1.0.0

## Major changes

- the dashboard now works with the standard e-mission database. To continue to use the dashboard for `rciti` branches please checkout the d41daeca03e42d053ecadf5fe4b84a861ce36aeb commit on master.

## Features

- `run_app()` gains an `anon_locations` argument which allows the user ids of sensed trips to be anonymously displayed on the map.

# emdash 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
