#' Create a **tblr** table object
#'
#' This function can be used to initialize a dataframe as a table object of class `tblr`. Usually, this is just the starting point: Use the pipe and chain together functions to incrementally add styling and additional parts (such as notes) to your table.
#'
#' `tblr()` preserves the dataframe it is called upon so you can continue to use functions that operate on dataframes. This function only adds the class `tblr` to the object and saves some metadata about the table (such as column types) as attributes. This metadata determines how the table is rendered at the end of a workflow. If you modify columns after the call to `tblr()`, you risk breaking this link.
#'
#' For example, characters that have a special meaning in LaTeX are escaped only in those columns that were originally text-like (character strings or factors). If you apply a formatting function to convert a numeric column to a character vector between your call to `tblr()` and rendering, you are required to take care of escaping special characters yourself.
#'
#' @param df The `tibble` or `data.frame` to be typeset as a table.
#' @param type The type of LaTeX table environment. This can be either "simple" (the default), "float", or "break".
#' - "simple" produces an unnumbered and untitled table intended for simple tabular matter to appear in the run of the text (see CMOS 3.51).
#' - "float" produces a floating numbered table that accommodates both a title and table notes, similar to a `threeparttable` in LaTeX.
#' - "break" produces a numbered table that can be broken across pages instead of floating. It accommodates both a title and table notes, similar to a `longtable` in LaTeX.
#' @param caption Title of the table. Needs to be specified unless `type` is "simple". In LaTeX output, this title appears under the "caption" keyword.
#'
#' @return A `data.frame` of class `tblr`.
#' @export
#'
#' @examples
#' tblr(mtcars)
tblr <- function(df,
                 type = "simple",
                 caption = NULL) {

  # sanity checks
  type <- rlang::arg_match(type, c("simple", "float", "break"))
  if (type != "simple") stopifnot(!is_null(caption))

  # convert factor to character (to harmonize "text-like" column types)
  df <- df |> mutate(across(where(is.factor), as.character))

  # initialize "interface" and "options"
  interface <- list()
  options <- list()

  # initialize spanners (as empty tibble, same orientation as original df)
  spanners <- df |>
    ungroup() |>
    filter(FALSE) |>
    mutate(across(everything(), as.character))

  # set default theme options
  theme <- list(
    table_indent = TRUE,
    table_booktabs = TRUE,
    row_group_sep = "\\addlinespace",
    row_group_indent = TRUE,
    row_group_head = TRUE,
    row_group_head_alignment = "l",
    row_group_head_fontstyle = "\\textit",
    row_group_head_cmidrule = FALSE,
    row_group_head_skip_stub = FALSE
  )

  # identify group column (if any)
  stopifnot(length(group_vars(df)) <= 1L)
  is_group_var <- colnames(df) %in% group_vars(df)
  col_type <- if_else(is_group_var, "group", "default")
  df <- ungroup(df)

  # guess alignment based on column type
  alignment <- if_else(purrr::map_lgl(df, is.numeric), "r", "l")

  # find positions of text-like columns (only those will be escaped)
  is_text <- purrr::map_lgl(df, is.character)

  # make boxhead
  boxhead <- tibble(
    variable = colnames(df),
    type = col_type,
    alignment = alignment,
    label = gt::escape_latex(colnames(df)),
    is_text = is_text
  )

  # save caption in options if provided
  if (!is_null(caption)) options$caption <- caption

  structure(
    df,
    # inherit classes of dataframe (e.g., grouping structure)
    class = c("tblr", class(df)),
    type = type,
    boxhead = boxhead,
    interface = interface,
    options = options,
    spanners = spanners,
    theme = theme
  )
}
