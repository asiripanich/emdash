#' Combine two named lists
#'
#' @param x,y a named list. All the elements must be uniquely named.
#' @param replace a logical value, default as TRUE. If TRUE, all
#'  the `x` elements with names that exist in the `y` list will be
#'  replaced with the values in `y`.
#'
#' @export
#' @examples
#' x <- list(x = 1, y = 2)
#' y <- list(x = 2)
#' combine_named_lists(x, y, FALSE)
combine_named_lists <- function(x, y, replace = TRUE) {
  checkmate::assert_list(x, names = "unique")
  checkmate::assert_list(y, names = "unique")
  checkmate::assert_flag(replace)

  if (replace) {
    non_overlapping_names <- names(x)[!names(x) %in% names(y)]
    x <- x[non_overlapping_names]
  }

  checkmate::assert_list(c(x, y))
}
