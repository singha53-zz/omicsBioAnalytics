# testthat::test_that("Style should be lint-free", {
#   testthat::skip_if_not(
#     requireNamespace("lintr", quietly = TRUE),
#     message = "Package lintr must be installed!"
#   )
#   lintr::expect_lint_free(path = here::here(), exclusions = NULL)
# })
