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
  if (!all(map_lgl(kwargs, rlang::is_formula))) {
    rlang::abort(c(
      "Each spanner must be a two-sided formula.",
      i = 'For example: `set_column_spanner(c(height, mass) ~ "Group 1")`.'
    ))
  }

  # initialize new row with NAs
  new_spanner_row <- spanners[1,]
  new_spanner_row[1,] <- NA

  # the rendered (non-group) columns, in display order
  boxhead <- attr(x, "boxhead")
  default_vars <- boxhead$variable[boxhead$type == "default"]

  # replace NA with LHS, location specified by tidy-select
  for (i in seq_along(kwargs)) {
    expr <- rlang::f_lhs(kwargs[[i]])

    # a spanner must cover a contiguous block of displayed columns, otherwise
    # the rendered \SetCell would span the wrong (intervening) columns
    selected <- names(tidyselect::eval_select(expr, data = x))
    pos <- sort(match(intersect(selected, default_vars), default_vars))
    if (length(pos) > 1 && any(diff(pos) != 1L)) {
      rlang::abort(c(
        "A spanner must cover a contiguous range of columns.",
        x = glue::glue("`{deparse(expr)}` selects non-adjacent columns: {paste(selected, collapse = ', ')}."),
        i = "Use a separate spanner formula for each block of adjacent columns."
      ))
    }

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
