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
