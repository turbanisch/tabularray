# Snapshot tests pin the rendered LaTeX for the canonical examples (the README
# tables and every roxygen @example). They are the safety net: later changes to
# formatting, escaping, or dependencies must either leave these byte-identical or
# produce a reviewed, intentional snapshot diff.

test_that("README: simple table", {
  expect_snapshot(cat(as_latex(tblr(tatooine()))))
})

test_that("README: marked-up grouped table", {
  x <- tatooine() |>
    dplyr::mutate(sex = stringr::str_to_title(sex)) |>
    dplyr::group_by(sex) |>
    tblr(type = "float", caption = "Starwars Creatures from Tatooine") |>
    set_source_notes(
      Note = "Entry C3PO altered to test characters that have a special meaning in LaTeX.",
      Source = "R package \\texttt{dplyr}"
    ) |>
    set_alignment(height:birth_year ~ "X[r]") |>
    set_column_labels(
      name = "", height = "Height", mass = "Mass", birth_year = "Birth Year"
    ) |>
    set_theme(row_group_style = "panel") |>
    set_interface(width = "0.7\\linewidth") |>
    set_column_spanner(c(height, mass) ~ "Group 1", birth_year ~ "Group 2") |>
    set_column_spanner(!name ~ "All my vars")
  expect_snapshot(cat(as_latex(x)))
})

test_that("set_alignment example", {
  x <- tblr(countries()) |> set_alignment(value = "X", starts_with("c") ~ "c")
  expect_snapshot(cat(as_latex(x)))
})

test_that("set_column_labels example", {
  x <- tblr(countries()) |>
    set_column_labels(value = "\\textbf{Value}", ends_with("y") ~ "COUNTRY")
  expect_snapshot(cat(as_latex(x)))
})

test_that("set_column_spanner example", {
  x <- tblr(countries()) |> set_column_spanner(!value ~ "Region")
  expect_snapshot(cat(as_latex(x)))
})

test_that("set_source_notes example", {
  x <- tblr(countries(), type = "float", caption = "A table with notes") |>
    set_source_notes(Source = "Built-in R dataset", Note = "Comes with R.")
  expect_snapshot(cat(as_latex(x)))
})

test_that("set_theme grouped example", {
  x <- countries2() |>
    dplyr::group_by(continent) |>
    tblr() |>
    set_theme(row_group_style = "panel", row_group_head_alignment = "l")
  expect_snapshot(cat(as_latex(x)))
})

test_that("row_group_style presets", {
  for (style in c("left", "center", "separators", "panel")) {
    x <- countries2() |>
      dplyr::group_by(continent) |>
      tblr() |>
      set_theme(row_group_style = style)
    expect_snapshot(cat(as_latex(x)), variant = style)
  }
})

test_that("table types: float and break", {
  expect_snapshot(cat(as_latex(tblr(countries(), type = "float", caption = "Float"))))
  expect_snapshot(cat(as_latex(tblr(countries(), type = "break", caption = "Break"))))
})

test_that("non-booktabs theme", {
  x <- tblr(countries()) |> set_theme(table_booktabs = FALSE)
  expect_snapshot(cat(as_latex(x)))
})
