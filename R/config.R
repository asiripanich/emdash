#' Save the default app config file
#'
#' This function saves the default app configuration file required by [run_app()].
#' Alternatively, you can download the file from
#' https://github.com/asiripanich/emdash/blob/master/inst/config-default.yml.
#'
#' @param dir_out the directory to save the config file
#' @return a logical value, `TRUE` if the file has been successfully saved.
#' @export
save_config_file <- function(dir_out) {
  checkmate::assert_directory(dir_out, access = "wr")
  file.copy(app_sys("config-default.yml"), dir_out)
}
