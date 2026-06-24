# LaTeX escaping behaviour for text columns. These assertions encode the
# *intended* contract so they survive the Phase 4 switch away from gt::escape_latex
# and the Phase 3 escaping refactor.

test_that("escape_latex maps every special character (gt parity)", {
  # expected values captured from gt::escape_latex() at the time of vendoring
  expect_identical(escape_latex("a&b"), "a\\&b")
  expect_identical(escape_latex("50%"), "50\\%")
  expect_identical(escape_latex("x_y"), "x\\_y")
  expect_identical(escape_latex("#1"), "\\#1")
  expect_identical(escape_latex("a$b"), "a\\$b")
  expect_identical(escape_latex("{c}"), "\\{c\\}")
  expect_identical(escape_latex("ti~ld"), "ti\\textasciitilde{}ld")
  expect_identical(escape_latex("ca^ret"), "ca\\textasciicircum{}ret")
  expect_identical(escape_latex("back\\slash"), "back\\textbackslash{}slash")
})

test_that("each special character escapes exactly once (no double-escaping)", {
  # the backslash introduced by escaping & must not itself be re-escaped
  expect_identical(escape_latex("&"), "\\&")
  expect_identical(escape_latex("_"), "\\_")
})

test_that("escape_latex preserves NA and empty input", {
  expect_identical(escape_latex(NA_character_), NA_character_)
  expect_identical(escape_latex(c("a$b", NA)), c("a\\$b", NA))
  expect_identical(escape_latex(character(0)), character(0))
})

test_that("special characters in text columns are escaped", {
  df <- tibble::tibble(label = c("C$PO", "50%", "a_b", "x&y", "#1"))
  out <- as_latex(tblr(df))
  expect_match(out, "C\\\\\\$PO")   # $ -> \$
  expect_match(out, "50\\\\%")      # % -> \%
  expect_match(out, "a\\\\_b")      # _ -> \_
  expect_match(out, "x\\\\&y")      # & -> \& (and must not be read as a column sep)
  expect_match(out, "\\\\#1")       # # -> \#
})

test_that("column labels escape the original column names", {
  df <- tibble::tibble(`a_b` = 1, `c%d` = 2)
  out <- as_latex(tblr(df))
  expect_match(out, "a\\\\_b")
  expect_match(out, "c\\\\%d")
})

test_that("NA in a text column renders as literal NA", {
  df <- tibble::tibble(label = c("a", NA, "b"))
  out <- as_latex(tblr(df))
  expect_match(out, "NA")
})

test_that("escaping is re-derived at render, not snapshotted at tblr()", {
  # a column that becomes character AFTER tblr() must still be escaped (the old
  # is_text snapshot left such columns unescaped -> broken LaTeX)
  x <- tblr(tibble::tibble(v = c(100, 200)))
  x[["v"]] <- c("a$b", "c%d")
  out <- as_latex(x)
  expect_match(out, "a\\\\\\$b")
  expect_match(out, "c\\\\%d")
})

test_that("raw LaTeX in labels/spanners is NOT escaped", {
  out <- as_latex(
    tblr(countries()) |>
      set_column_labels(value = "\\textbf{Value}")
  )
  expect_match(out, "\\\\textbf\\{Value\\}", fixed = FALSE)
})
