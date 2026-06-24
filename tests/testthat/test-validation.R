# Input validation and informative errors/warnings.

test_that("non-tblr input is rejected with a helpful message", {
  expect_error(set_colspec(mtcars, mpg = "c"), "must be a `tblr` object")
  expect_error(tblr_as_latex(mtcars), "must be a `tblr` object")
})

test_that("non-simple types require a caption", {
  expect_error(tblr(countries(), type = "float"), "caption.*required")
  expect_error(tblr(countries(), type = "break"), "caption.*required")
  expect_no_error(tblr(countries(), type = "float", caption = "C"))
})

test_that("at most one grouping variable is allowed", {
  expect_error(
    countries2() |> dplyr::group_by(continent, country) |> tblr(),
    "at most one grouping variable"
  )
})

test_that("setting colspec via set_interface() is rejected at render", {
  expect_error(
    tblr(countries()) |> set_interface(colspec = "lll") |> tblr_as_latex(),
    "column specification"
  )
})

test_that("source notes require a float or break table", {
  expect_error(
    tblr(countries()) |> set_source_notes(Note = "x"),
    "only supported for"
  )
})

test_that("spanners must be formulas", {
  expect_error(
    tblr(countries()) |> set_column_spanner("Region"),
    "two-sided formula"
  )
})

test_that("unknown column names in set_*() warn instead of silently no-op", {
  expect_warning(
    tblr(countries()) |> set_column_labels(nonexistent = "X"),
    "unknown column"
  )
  expect_warning(
    tblr(countries()) |> set_colspec(nope = "c"),
    "unknown column"
  )
})
