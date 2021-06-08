<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->
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
