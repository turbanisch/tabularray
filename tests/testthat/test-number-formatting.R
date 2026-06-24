# Behaviour of the default numeric/Date formatting at render time.
# Encodes the intended contract (fixed decimal places, type-aware), in
# particular the owner-reported regression: 3.04 must not become "3".

# render a single numeric column and return the whole LaTeX string
fmt1 <- function(x) as_latex(tblr(tibble::tibble(v = x)))

test_that("doubles get fixed 2 decimals (3.04 -> 3 regression)", {
  out <- fmt1(c(3.04, 1.2, 10.5))
  expect_match(out, "3.04", fixed = TRUE)
  expect_match(out, "1.20", fixed = TRUE)
  expect_match(out, "10.50", fixed = TRUE)
})

test_that("large-magnitude doubles keep their decimals", {
  # the old significant-figures default dropped these to "123" / "100"
  out <- fmt1(c(123.456, 100.50))
  expect_match(out, "123.46", fixed = TRUE)
  expect_match(out, "100.50", fixed = TRUE)
})

test_that("integer columns render without decimals", {
  out <- fmt1(1:3)
  expect_no_match(out, "[0-9]\\.[0-9]")
})

test_that("whole-valued doubles render without decimals", {
  out <- fmt1(c(2019, 2020, 2021))
  expect_match(out, "2019", fixed = TRUE)
  expect_no_match(out, "2019\\.")
})

test_that("no scientific notation for very small or large values", {
  out <- fmt1(c(0.001, 1000000.5))
  expect_no_match(out, "[0-9]e[+-]?[0-9]")
})

test_that("formatting is independent of the other values in the column", {
  # the old format(digits=) default was composition-dependent: the same number
  # rendered differently depending on its column-mates. It must not be.
  expect_match(fmt1(c(22.8, 21.0)), "22.80", fixed = TRUE)
  expect_match(fmt1(c(22.8, 0.001)), "22.80", fixed = TRUE)
})

test_that("Date columns render as ISO dates", {
  df <- tibble::tibble(d = as.Date("2020-01-15") + 0:2)
  out <- as_latex(tblr(df))
  expect_match(out, "2020-01-15", fixed = TRUE)
  expect_match(out, "2020-01-17", fixed = TRUE)
})
