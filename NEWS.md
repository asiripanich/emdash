<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# emdash 1.4.1.9001

- Fixes bad column names that can crash the dashboard on the first reload of the trip data. (#57)


# emdash 1.4.1.9000

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
