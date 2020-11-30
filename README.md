
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emdash

<!-- badges: start -->

[![R build
status](https://github.com/asiripanich/emdash/workflows/R-CMD-check/badge.svg)](https://github.com/asiripanich/emdash/actions)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
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

Alternatively, with Docker `docker pull amarins/emdash`.

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

## Loading test data

This repository contains a test mongodump with entries from the
[e-mission unit test real
data](https://github.com/e-mission/e-mission-server/tree/master/emission/tests/data/real_examples).

  - 2016-07-22
  - 2016-07-25
  - 2016-07-27
  - 2016-08-04
  - 2016-08-05
  - 2016-08-09
  - 2016-08-10
  - 2016-08-11

It includes background sensed data (`background/*`), analysis results
(`analysis/*`) and the mode and purpose confirm objects from the master
branch (`manual/mode_confirm` and `manual/purpose_confirm`).

To load the data into a local mongodb,

    $ unzip test-data/2016_jul_aug_test_trips.zip
    $ mongorestore

If you would like to load additional data, you can do so through the
[e-mission
server](https://github.com/e-mission/e-mission-server/#loading-test-data).

For convenience, you may use this `docker_start_test_mongodb.sh` for
starting up a Docker container that will run the test database.

### Use with Docker

We provide a Dockerfile to run this dashboard. In the future we may
include this as part of an e-mission docker-compose file which will be
hosted on [the e-mission-docker github
repo](https://github.com/e-mission/e-mission-docker).

Be warned that the emdash Docker image is quite large, around 2.8 GB,
and take a while to build. You can use pull from [the emdash docker
repository](https://hub.docker.com/r/amarins/emdash) and skip the wait\!

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
