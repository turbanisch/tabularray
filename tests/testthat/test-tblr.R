test_that("class properly set", {
  expect_equal(class(tblr(mtcars)), c("tblr", "data.frame"))
})
