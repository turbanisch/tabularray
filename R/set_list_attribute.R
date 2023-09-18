# example -----------------------------------------------------------------

# x |>
#   set_interface(
#     hlines = character(0),
#     vlines = c("1,3,5", "dashed"),
#     rows = "7mm"
#   )

# {
#   hlines,
#   vlines = {1, 3, 5}{dashed},
#   rows = {7mm}
# }

# assign key-value pairs as elements of list stored in attribute

# main function -----------------------------------------------------------

set_list_attribute <- function(x, attribute_name, names_prefix = NULL, ...) {

  stop_if_not_tblr(x)
  dots_list <- rlang::list2(...)

  # prefix all names in the list with a common string, e.g. for remark{...}
  if (!is_null(names_prefix)) {
    names(dots_list) <- str_c(names_prefix, "{", names(dots_list), "}")
  }

  attr(x, attribute_name) <- purrr::list_assign(
    attr(x, attribute_name),
    !!!dots_list
  )

  return(x)
}


# wrapper functions to set list attributes --------------------------------

#' Set table interface
#'
#' This function gives you low-level access to the mandatory argument of a `tblr` environment, also called "inner specifications". In LaTeX, this section is primarily used to define the column specification. The `tabularray` LaTeX package provides a so-called "new interface" that extends the functionality of the mandatory argument and allows targeting individual rows, columns, and cells.
#'
#' The mandatory argument is a list of (mostly) key-value pairs. Use named arguments to specify them with this function. Both the names and values you provide are assumed to be raw LaTeX strings; characters that have a special meaning in LaTeX will not be escaped. To set a key without a value (e.g., "hlines"), pass `character(0)` as in the example below. Note that the value can be a vector with more than one element. Each element is automatically wrapped in curly braces.
#'
#' Shortcut functions targeting specific parts of the interface such as `set_alignment()` take precedence over the specifications you make here.
#'
#' @param x A `tblr` table object.
#' @param ... Named arguments to be spliced into key-value pairs within the mandatory argument of a `tblr` environment.
#'
#' @return A `tblr` table object.
#' @export
#'
#' @examples
#' # Produce the following specification in LaTeX:
#' # {
#' #   hlines,
#' #   vlines = {1, 3, 5}{dashed},
#' #   rows = {7mm}
#' # }
#'
#' \dontrun{
#' x |>
#'   set_interface(
#'     hlines = character(0),
#'     vlines = c("1,3,5", "dashed"),
#'     rows = "7mm"
#'   )
#' }
set_interface <- function(x, ...) {
  set_list_attribute(x, "interface", ...)
}

#' Set table options
#'
#' This function gives you low-level access to the optional argument of a `tblr` environment, also called "outer specifications". For example, for types "float" and "break" the optional argument can contain the caption, a label, and notes.
#'
#' The optional argument is a list of key-value pairs. Use named arguments to specify them with this function. Both the names and values you provide are assumed to be raw LaTeX strings; characters that have a special meaning in LaTeX will not be escaped. Each value is automatically wrapped in curly braces.
#'
#' Shortcut functions targeting specific options such as the caption you specify in the call to `tblr()` take precedence over the specifications you make here.
#'
#' @param x A `tblr` table object.
#' @param ... Named arguments to be spliced into key-value pairs within the mandatory argument of a `tblr` environment.
#'
#' @return A `tblr` table object.
#' @export
set_options <- function(x, ...) {
  set_list_attribute(x, "options", ...)
}

#' Add source notes
#'
#' Add table notes to your table. Only `tblr` table objects of type "float" or "break" support table notes.
#'
#' @param x A `tblr` table object of type "float" or "break".
#' @param ... Named arguments to add notes under a table. Names provide a label for each note; the argument value is the actual note itself.
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
#'x <- tblr(df, type = "float", caption = "A table with notes")
#'
#'x |> set_source_notes(
#'  Source = "Built-in R dataset",
#'  Note = "This dataset comes with every R installation and is available without explicitly loading it."
#')
set_source_notes <- function(x, ...) {
  stopifnot(attr(x,"type") %in% c("float", "break"))
  set_list_attribute(x, "options", names_prefix = "remark", ...)
}
