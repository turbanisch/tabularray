print.tblr <- function(x) {
  writeLines(tblr_as_latex(x))
}

knit_print.tblr <- function(x, ...) {
  x <- tblr_as_latex(x)
  knitr::knit_print(x, ...)
}