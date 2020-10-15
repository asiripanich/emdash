
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emdash

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/asiripanich/emdash/workflows/R-CMD-check/badge.svg)](https://github.com/asiripanich/emdash/actions)
<!-- badges: end -->

The goal of emdash is to provide a deployer dashboard for an e-mission
deployment.

The dashboard is structured as follow:

  - Dashboard
      - Value boxes
          - Number of users (+ daily)
          - Number of active users today
          - Number of trips (+ daily)
          - Time since first user
      - Plots
          - Sign ups
          - Trips
          - Branch
          - Platform

<img src="man/figures/emdash_dashboard.gif" ></img>

  - Tables
      - Interactive plot (linked with the tables)
      - Tables (downloadable in various formats from the UI)
          - Participants
          - Trips

<img src="man/figures/emdash_tables.gif" ></img>

  - Maps
      - Trips (with a data filter on the left side)

<img src="man/figures/emdash_map.gif" ></img>

## Installation

You can install emdash from GitHub:

``` r
install.packages("remotes")
remotes::install_github("asiripanich/emdash")
```

## WARNING\!

This package has been tested only on the following e-mission-phone
branches: `rciti1`, `rciti2`, and `rciti3`. There are query functions
that only work on those branches hence it is likely that the dashboard
will crash if you use other branches. I will find time to generalise the
dashboard or at least make it work with e-mission-phone’s `master`
branch in September. However, it shouldn’t be hard to modify this
dashboard specific to your e-mission deployment, granted that you know R
and Shiny.

## How to use the dashboard

### Run locally

To use the dashboard you must config the port of your e-mission mongoDB
in the `emdash::run_app(mongo_url = 'mongodb://localhost:27017')`
command. As you can see, the default is `27017`. The easiest way to use
this dashboard with your e-mission deployment is to create a SSH tunnel
to the e-mission mongo on your e-mission server.

``` ssh
ssh -L {my-local-port}:localhost:{remote-server-port} username@your-server-ip-address-or-domain-name
ssh -L 27017:localhost:27017 ec2-user@123.12.12.1
```

Here is how you launch a e-mission dashboard with emdash.

``` r
library(emdash)
emdash::run_app(mongo_url = 'mongodb://localhost:27017')
```

### Use with Docker

We provide a Dockerfile to run this dashboard. In the future we may
include this as part of an e-mission docker-compose file which will be
hosted on [the e-mission-docker github
repo](https://github.com/e-mission/e-mission-docker).

Be warned that the emdash Docker image is quite large, around 2.8 GB,
and take a while to build.

## Loading test data

This repository contains a test mongodump with entries from the [e-mission unit test real data](https://github.com/e-mission/e-mission-server/tree/master/emission/tests/data/real_examples).
- testuser:
    - 2016-07-22
    - 2016-07-25
    - 2016-07-27
    - 2016-08-04
    - 2016-08-05
    - 2016-08-09
    - 2016-08-10
    - 2016-08-11
- testanotheruser:
    - 2015-07-22
    - 2015-08-21
    - 2015-08-23
    - 2015-08-27

It includes background sensed data (`background/*`), analysis results
(`analysis/*`) and the mode and purpose confirm objects from the master branch
(`manual/mode_confirm` and `manual/purpose_confirm`).

To load the data into a local mongodb,

```
$ unzip data/2015_2016_jul_aug_test_trips.zip
$ mongorestore
```

If you would like to load additional data, you can do so through the [e-mission server](https://github.com/e-mission/e-mission-server/#loading-test-data).

## How to customise the dashboard

First you need to know [R](https://www.r-project.org/) and
[Shiny](https://shiny.rstudio.com/). I tried to modularise each
component as much as possible so that the dashboard is easy to be
customised. Any advices to improve the dashboard are welcomed. :)

### Customise query functions

See `R/utils_query_database.R`.

The functions in this script are for querying data from the e-mission
mongoDB database.

### Customise data

See `R/utils_tidy_data.R`.

The functions in this script are for cleansing and transforming data
queried using the functions in `R/utils_query_database.R`.
