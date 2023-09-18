# main function -----------------------------------------------------------

set_boxhead_property <- function(x, property, ...) {

  stop_if_not_tblr(x)
  boxhead <- attr(x, "boxhead")
  kwargs <- rlang::list2(...)

  for (i in seq_along(kwargs)) {

    if (rlang::is_formula(kwargs[[i]])) {
      # interpret LHS as tidy-select if element is a formula
      # no need to use `enquo()`, LHS is already a symbol
      expr <- rlang::f_lhs(kwargs[[i]])
      target_col_positions <- tidyselect::eval_select(expr, data = x)
      target_col_names <- names(target_col_positions)
      replacement <- rlang::f_rhs(kwargs[[i]])
    } else {
      # otherwise interpret as named list element
      # note that names are a property of the entire list, not of an individual element
      target_col_names <- names(kwargs)[i]
      replacement <- kwargs[[i]]
    }

    # replace value in the property-column in rows corresponding to those variables
    boxhead[boxhead$variable %in% target_col_names, property] <- replacement
  }

  attr(x, "boxhead") <- boxhead
  return(x)
}


# wrapper functions -------------------------------------------------------

#' Set column alignment
#'
#' Set the column alignment and other `colspec` properties. By default, columns will be either left-aligned ("l") or right-aligned ("r"), if they are numeric.
#'
#' Column specifications can be more sophisticated than just left ("l"), center ("c"), and right ("r"). In `tabularray`, all column types are derived from the column type `Q`. Useful shorthands include `X` (to evenly distribute columns given a fixed table width) and `S` (to format columns using `siunitx`).
#'
#' @param x A `tblr` table object.
#' @param ... Expressions for the assignment of column alignment (and column specifications more generally). Expressions can be named arguments of the form `<column name> = <alignment>` or make use of tidy-select. When using tidy-select, expressions have to be two-sided formulas (i.e., of the form `<LHS> ~ <RHS>`). In this case, the left-hand side is used to identify selections of columns and the right-hand side to specify the column alignment. Note that the column specifications you supply are assumed to be raw LaTeX strings; characters that have a special meaning in LaTeX will not be escaped.
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
#'   set_alignment(value = "X", starts_with("c") ~ "c")
set_alignment <- function(x, ...) {
  set_boxhead_property(x = x, property = "alignment", ...)
}

#' Set column labels
#'
#' Define column labels (headers). By default, column labels are derived from the column names of the underlying dataframe. However, unlike those column names, column labels do not have to be unique. Note that column labels are only used for rendering the table; other functions still use the original column names to refer to columns of the underlying dataframe.
#'
#' @param x A `tblr` table object.
#' @param ... Expressions for the assignment of column labels. Expressions can be named arguments of the form `<column name> = <label>` or make use of tidy-select. When using tidy-select, expressions have to be two-sided formulas (i.e., of the form `<LHS> ~ <RHS>`). In this case, the left-hand side is used to identify selections of columns and the right-hand side to specify the column label. Note that the column labels you specify are assumed to be raw LaTeX strings; characters that have a special meaning in LaTeX will not be escaped.
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
#'   set_column_labels(value = "\\textbf{Value}", ends_with("y") ~ "COUNTRY")

set_column_labels <- function(x, ...) {
  set_boxhead_property(x = x, property = "label", ...)
}
