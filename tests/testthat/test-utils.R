test_that("util functions", {

  x <- list(x = 1, y = 2)
  y <- list(x = 2)
  checkmate::expect_list(
    combine_named_lists(x, y, FALSE),
    len = length(x) + length(y)
  )
  checkmate::expect_list(
    combine_named_lists(x, y, TRUE),
    len = length(x)
  )

})
