# Constructor and object-structure tests for tblr().

test_that("class properly set", {
  expect_equal(class(tblr(mtcars)), c("tblr", "data.frame"))
})

test_that("tblr() stores the expected attributes", {
  x <- tblr(countries())
  expect_s3_class(attr(x, "boxhead"), "data.frame")
  expect_setequal(attr(x, "boxhead")$variable, names(countries()))
  expect_type(attr(x, "interface"), "list")
  expect_type(attr(x, "options"), "list")
  expect_type(attr(x, "theme"), "list")
})

test_that("default alignment is right for numeric, left otherwise", {
  bh <- attr(tblr(countries()), "boxhead")
  expect_equal(bh$alignment[bh$variable == "value"], "r")
  expect_equal(bh$alignment[bh$variable == "country"], "l")
})

test_that("type is validated and caption required for non-simple types", {
  expect_error(tblr(countries(), type = "nonsense"))
  expect_error(tblr(countries(), type = "float"))   # caption missing
  expect_no_error(tblr(countries(), type = "float", caption = "C"))
})

test_that("factors are harmonised to character", {
  df <- tibble::tibble(f = factor(c("a", "b")), n = 1:2)
  x <- tblr(df)
  expect_type(x$f, "character")
})

test_that("grouping is recorded as a group column and at most one is allowed", {
  x <- countries2() |> dplyr::group_by(continent) |> tblr()
  bh <- attr(x, "boxhead")
  expect_equal(bh$type[bh$variable == "continent"], "group")
  expect_error(
    countries2() |> dplyr::group_by(continent, country) |> tblr()
  )
})

test_that("tblr_as_latex returns a knit_asis string", {
  out <- tblr_as_latex(tblr(countries()))
  expect_s3_class(out, "knit_asis")
})
