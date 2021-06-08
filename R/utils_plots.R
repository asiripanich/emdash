#' Plots
#'
#' @description
#'
#' Plotting functions.
#'
#' @param participants a data.frame object.
#' @param trips a data.frame object.
#'
#' @return a ggplot object
#' @noRd
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_bar theme_minimal
#' @importFrom ggplot2 labs scale_x_date scale_fill_brewer
utils_plot_signup_trend <- function(participants) {
  participants %>%
    dplyr::transmute(date = lubridate::date(update_ts)) %>%
    ggplot(aes(x = date)) +
    geom_bar(fill = "#0c4c8a") +
    theme_minimal()
}

# @rdname utils_plot
utils_plot_trip_trend <- function(trips) {
  trips %>%
    dplyr::transmute(date = lubridate::date(start_fmt_time)) %>%
    ggplot(aes(x = date, label = as.character(date))) +
    geom_bar(fill = "#0c4c8a") +
    ggplot2::scale_x_date() +
    theme_minimal()
}

# @rdname utils_plot
utils_plot_trips_last_seven_days <- function(trips) {
  trips %>%
    dplyr::transmute(date = lubridate::date(start_fmt_time)) %>%
    ggplot() +
    aes(x = date) +
    geom_bar(fill = "#0c4c8a") +
    ggplot2::scale_x_date(limits = c(Sys.Date() - 7, NA)) +
    labs(x = "Date", y = "Count")
  theme_minimal()
}

# @rdname utils_plot
utils_plot_branch <- function(participants) {
  participants %>%
    ggplot(aes(x = branch, fill = branch)) +
    geom_bar() +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    labs(x = "", y = "Count") +
    theme_minimal()
}

# @rdname utils_plot
utils_plot_participation_period <- function(participants) {
  participants %>%
    ggplot(aes(x = round(n_days))) +
    geom_bar(fill = "#0c4c8a") +
    labs(x = "Number of days", y = "Count") +
    theme_minimal()
}

# @rdname utils_plot
utils_plot_platform <- function(participants) {
  participants %>%
    ggplot(aes(x = curr_platform, fill = curr_platform)) +
    geom_bar() +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    labs(x = "", y = "Count") +
    theme_minimal()
}
