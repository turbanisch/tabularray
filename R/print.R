#' Print a **tblr** table object
#'
#' This function uses `tblr_as_latex` to generate LaTeX markup from the `tblr` table object and prints it to the console.
#'
#' @param x A `tblr` table object.
#' @param ... Any additional parameters.
#'
#' @return A character string containing LaTeX markup.
#' @export
print.tblr <- function(x, ...) {
  writeLines(tblr_as_latex(x))
}

#' Render a **tblr** table object with knitr
#'
#' This function instructs `knitr` to use `tblr_as_latex` to generate LaTeX markup from the `tblr` table object.
#'
#' @param x A `tblr` table object
#' @param ... Any additional parameters passed to `knitr::knit_print`
#'
#' @return A character string containing LaTeX markup of class `knit_asis`.
#' @keywords internal
#' @noRd
#' @exportS3Method knitr::knit_print
knit_print.tblr <- function(x, ...) {
  x <- tblr_as_latex(x)
  knitr::knit_print(x, ...)
}
