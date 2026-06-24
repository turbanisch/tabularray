# Regression + structural tests for column spanners.

test_that("spanners render without error (asplit 1D-array regression)", {
  # asplit(MARGIN = 1) yields 1D-array row slices; modern dplyr::if_else()
  # rejects arrays as a condition. This must not error.
  expect_no_error(
    tblr(countries()) |>
      set_column_spanner(!value ~ "Region") |>
      tblr_as_latex()
  )
})

test_that("a single spanner produces a SetCell with the right column span", {
  out <- as_latex(tblr(countries()) |> set_column_spanner(!value ~ "Region"))
  # "Region" spans the two leading columns (continent, country)
  expect_match(out, "\\\\SetCell\\[c=2\\]\\{c\\} Region")
})

test_that("stacked spanners add one row each, bottom-up", {
  x <- tblr(countries()) |>
    set_column_spanner(c(continent, country) ~ "Bottom") |>
    set_column_spanner(everything() ~ "Top")
  # call once per spanner row -> two spanner attribute rows
  expect_equal(nrow(attr(x, "spanners")), 2L)
  out <- as_latex(x)
  expect_match(out, "Top")
  expect_match(out, "Bottom")
})

test_that("spanners draw cmidrules under spanned columns", {
  out <- as_latex(tblr(countries()) |> set_column_spanner(!value ~ "Region"))
  expect_match(out, "\\\\cmidrule")
})
