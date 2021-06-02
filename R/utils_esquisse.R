drop_list_columns <- function(.data) {
  stopifnot(is.data.frame(.data))
  subset(.data, select = !(sapply(.data, typeof) == "list"))
}
