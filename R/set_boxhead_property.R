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
      target_col_names <- if (is.null(names(kwargs))) NA_character_ else names(kwargs)[i]
      replacement <- kwargs[[i]]

      # an unnamed argument can't target a column; fail clearly rather than with
      # an opaque recycling error further down
      if (is.na(target_col_names) || target_col_names == "") {
        rlang::abort(c(
          "Each argument must be named or a two-sided formula.",
          x = "Got an unnamed argument.",
          i = "Use `column = value` to target a column by name, or `selection ~ value` for tidy-select."
        ))
      }

      # the tidy-select path errors on unknown columns via eval_select(); guard
      # the named-argument path, which would otherwise silently do nothing
      unknown <- setdiff(target_col_names, boxhead$variable)
      if (length(unknown)) {
        rlang::warn(glue::glue(
          "Ignoring unknown column{if (length(unknown) > 1L) 's' else ''}: ",
          "{paste(unknown, collapse = ', ')}."
        ))
      }
    }

    # replace value in the property-column in rows corresponding to those variables
    boxhead[boxhead$variable %in% target_col_names, property] <- replacement
  }

  attr(x, "boxhead") <- boxhead
  return(x)
}


# wrapper functions -------------------------------------------------------

#' Set column specifications
#'
#' Set the column specification (`colspec`) of one or more columns. The column specification controls alignment and, more generally, the `tabularray` column type. By default, columns are left-aligned ("l"), or right-aligned ("r") if they are numeric.
#'
#' Column specifications can be more than just left ("l"), center ("c"), and right ("r"). In `tabularray`, all column types are derived from the column type `Q`. Useful shorthands include `X` (to evenly distribute columns given a fixed table width) and `S` (to format columns using `siunitx`, e.g. `"S[table-format=2.3]"`).
#'
#' @param x A `tblr` table object.
#' @param ... Expressions for the assignment of column specifications. Expressions can be named arguments of the form `<column name> = <colspec>` or make use of tidy-select. When using tidy-select, expressions have to be two-sided formulas (i.e., of the form `<LHS> ~ <RHS>`). In this case, the left-hand side is used to identify selections of columns and the right-hand side to specify the column specification. Note that the column specifications you supply are assumed to be raw LaTeX strings; characters that have a special meaning in LaTeX will not be escaped.
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
#'   set_colspec(value = "X", starts_with("c") ~ "c")
set_colspec <- function(x, ...) {
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
