# main function -----------------------------------------------------------

#' Set theme options
#'
#' This function allows you to fine-tune the way your `tblr` table object is rendered as LaTeX.
#'
#' @param x A `tblr` table object.
#' @param table_indent Slightly indent the table contents? `TRUE` by default. `FALSE` removes the indentation by adding "@\{\}" to both sides of the column specification in LaTeX.
#' @param table_booktabs Format the table using `booktabs`? `TRUE` by default. `booktabs` is a LaTeX package that modifies the width (thickness) of rules and spacing around them.
#' @param row_group_sep How to separate row groups? Default is `\\addlinespace`. Another common choice is `\\midrule`.  Specify `character(0)` to remove separators.
#' @param row_group_style A preset to style row groups, either `"left"`, `"center"`, or `"panel"`. These presets affect the options `row_group_indent`, `row_group_head_alignment`, `row_group_head_cmidrule`, and `row_group_head_skip_stub` but can be overwritten.
#' @param row_group_indent Indent rows under a group heading? `TRUE` by default. This value is affected by `row_group_style` presets but can be overwritten. Indentation is achieved by adding an invisible column that is left blank in grouped rows.
#' @param row_group_head_alignment The value for the alignment (column specification) of row group heads, `"l"` by default. This value is affected by `row_group_style` presets but can be overwritten.
#' @param row_group_head_fontstyle The font style to use for row group heads, `"\\textit"` (italic) by default. Other options include:
#' - `"\\textbf"`: boldface
#' - `"\\textup"`: standard font style
#' - `"\\textsc"`: small caps
#'
#' Note that only a single command can be used.
#' @param row_group_head_cmidrule Add a rule below the row group head? `FALSE` by default. This value is affected by `row_group_style` presets but can be overwritten.
#' @param row_group_head_skip_stub Should the stub (i.e., the first column) be skipped when aligning row group heads and drawing rules underneath them? `FALSE` by default. This value is affected by `row_group_style` presets but can be overwritten.
#'
#' @return A `tblr` table object.
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- tibble(
#'   continent = c("Europe", "Asia", "Asia", "Europe", "Asia"),
#'   country = c("Germany","China", "Afghanistan","France","Taiwan"),
#'   gdp = c(0.17, 0.23, 11.3, 17, 2.4),
#'   population = c(11.0, 7.3, 123.11, 5, 33)
#' )
#'
#' # apply preset "panel" but overwrite to left-align
#' df |>
#'   group_by(continent) |>
#'   tblr() |>
#'   set_theme(
#'     row_group_style = "panel",
#'     row_group_head_alignment = "l"
#'   )
set_theme <- function(
  x,
  table_indent = NULL,
  table_booktabs = NULL,
  row_group_sep = NULL,
  row_group_style = NULL,
  row_group_indent = NULL,
  row_group_head_alignment = NULL,
  row_group_head_fontstyle = NULL,
  row_group_head_cmidrule = NULL,
  row_group_head_skip_stub = NULL
) {

  stop_if_not_tblr(x)

  # turn argument name-value pairs into list (excluding data argument and NULL arguments)
  arg_names <- setdiff(
    names(formals(set_theme)),
    "x"
  )
  arg_vals <- mget(arg_names) |> discard(is_null)

  # expand meta theme options (more specific options survive if set)
  arg_vals <- expand_meta_theme_options(arg_vals)

  # overwrite theme options in tblr object
  attr(x, "theme") <- purrr::list_assign(
    attr(x, "theme"),
    !!!arg_vals
  )

  return(x)
}


# helper functions --------------------------------------------------------

expand_meta_theme_options <- function(arg_vals) {

  if (!"row_group_style" %in% names(arg_vals)) return(arg_vals)

  if (arg_vals$row_group_style == "left") {
    # left
    replacements <- list(
      row_group_indent = TRUE,
      row_group_head_alignment = "l",
      row_group_head_cmidrule = FALSE,
      row_group_head_skip_stub = FALSE
    )
  } else if (arg_vals$row_group_style == "center") {
    # center
    replacements <- list(
      row_group_indent = FALSE,
      row_group_head_alignment = "c",
      row_group_head_cmidrule = FALSE,
      row_group_head_skip_stub = FALSE
    )
  } else {
    # panel
    replacements <- list(
      row_group_indent = FALSE,
      row_group_head_alignment = "c",
      row_group_head_cmidrule = TRUE,
      row_group_head_skip_stub = TRUE
    )
  }

  # add back non-meta theme options (these take precedence over meta options)
  other_arg_vals <- arg_vals |> discard_at("row_group_style")
  out <- purrr::list_assign(
    replacements,
    !!!other_arg_vals
  )
  out
}
