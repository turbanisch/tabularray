#' documentation
#' type: The type of LaTeX table environment. This can be either "simple" (the default), "float", or "break". The corresponding LaTeX environments defined in the **tabularray** package are "tblr", "talltblr", and "longtblr". "simple" produces an unnumbered and untitled table intended for simple tabular matter to appear in the run of the text (see CMOS 3.51). "float" produces a floating numbered table that accommodates both a title and table notes, similar to a "threeparttable" in LaTeX. "break" produces a numbered table that can be broken across pages instead of floating. It accommodates both a title and table notes, similar to a "longtable" in LaTeX. Could use table: simple + float + break ~ tabularray environment + traditional LaTeX environment + Description
#' booktabs: Should tables be formatted with booktabs styling? This setting modifies the thickness of rules (horizontal borders) and spacing between lines. If `TRUE` (the default), the booktabs versions of the LaTeX table environments corresponding to "type" will be used. These are "booktabs", "talltabs", and "longtabs". Could use table: plain + booktabs ~ simple + float + break
#' caption: Title of the table. Needs to be specified unless `type` is "simple". In LaTeX output, this title appears under the "caption" keyword.

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
  
  # set default theme options
  theme <- list(
    table_indent = TRUE,
    table_booktabs = TRUE,
    row_group_sep = "\\addlinespace",
    row_group_indent = TRUE,
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
  alignment <- if_else(map_lgl(df, is.numeric), "r", "l")
  
  # find positions of text-like columns (only those will be escaped)
  is_text <- map_lgl(df, is.character)
  
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
    theme = theme
  )
}