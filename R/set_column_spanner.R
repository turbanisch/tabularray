#' Set column spanners
#'
#' Set column spanner labels. Column spanner labels are labels that appear above column labels (headers) and span several of them. Call this function once for each spanner row in the table, starting with the one at the bottom.
#'
#' @param x A `tblr` table object.
#' @param ... Expressions for the assignment of column spanner labels. Expressions have to be two-sided formulas (i.e., of the form `<LHS> ~ <RHS>`). The left-hand side is used to identify selections of columns via tidy-select and the right-hand side to specify the column spanner label. Note that the column spanner labels you provide are assumed to be raw LaTeX strings; characters that have a special meaning in LaTeX will not be escaped.
#'
#' @return A `tblr` table object.
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- tibble(
#'   continent = c("Europe", "Asia", "Asia", "Europe", "Asia"),
#'   country = c("Germany","China", "Afghanistan","France","Taiwan"),
#'   value = c(0.17, 0.23, 11.3, 17, 2.4)
#' )
#'
#' tblr(df) |>
#'   set_column_spanner(!value ~ "Region")
set_column_spanner <- function(x, ...) {

  stop_if_not_tblr(x)
  spanners <- attr(x, "spanners")
  kwargs <- rlang::list2(...)
  stopifnot(all(map_lgl(kwargs, rlang::is_formula)))

  # initialize new row with NAs
  new_spanner_row <- spanners[1,]
  new_spanner_row[1,] <- NA

  # replace NA with LHS, location specified by tidy-select
  for (i in seq_along(kwargs)) {
    expr <- rlang::f_lhs(kwargs[[i]])
    new_spanner_row <- new_spanner_row |>
      mutate(across(
        {{ expr }},
        \(x) replace_na(x, rlang::f_rhs(kwargs[[i]]))
      ))
  }

  # save new column spanner row at top of df
  attr(x, "spanners") <- bind_rows(
    new_spanner_row,
    spanners
  )
  return(x)
}
