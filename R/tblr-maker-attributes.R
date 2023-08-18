#' documentation
#' type: The type of LaTeX table environment. This can be either "simple" (the default), "float", or "break". The corresponding LaTeX environments defined in the **tabularray** package are "tblr", "talltblr", and "longtblr". "simple" produces an unnumbered and untitled table intended for simple tabular matter to appear in the run of the text (see CMOS 3.51). "float" produces a floating numbered table that accommodates both a title and table notes, similar to a "threeparttable" in LaTeX. "break" produces a numbered table that can be broken across pages instead of floating. It accommodates both a title and table notes, similar to a "longtable" in LaTeX. Could use table: simple + float + break ~ tabularray environment + traditional LaTeX environment + Description
#' booktabs: Should tables be formatted with booktabs styling? This setting modifies the thickness of rules (horizontal borders) and spacing between lines. If `TRUE` (the default), the booktabs versions of the LaTeX table environments corresponding to "type" will be used. These are "booktabs", "talltabs", and "longtabs". Could use table: plain + booktabs ~ simple + float + break
#' caption: Title of the table. Needs to be specified unless `type` is "simple". In LaTeX output, this title appears under the "caption" keyword.

tblr <- function(df,
                 type = "simple",
                 booktabs = TRUE,
                 caption = NULL) {
  # sanity checks
  type <- rlang::arg_match(type, c("simple", "float", "break"))
  if (type != "simple") stopifnot(!is_null(caption))
  
  # convert factor to character (to unify "text-like" column types)
  df <- df |> mutate(across(where(is.factor), as.character))
  
  # initialize "interface" and "options" 
  interface <- list()
  options <- list()
  
  # identify group column (if any)
  stopifnot(length(group_vars(df)) <= 1L)
  is_group_var <- colnames(df) %in% group_vars(df)
  col_type <- if_else(is_group_var, "group", "default")
  
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
    class = c("tblr", "tbl_df", "tbl", "data.frame"),
    type = type,
    booktabs = booktabs,
    boxhead = boxhead,
    interface = interface,
    options = options
  )
}

tblr_as_latex <- function(x) {
  
  # add shortcuts to attributes
  type = attr(x, "type")
  booktabs = attr(x, "booktabs")
  boxhead = attr(x, "boxhead")
  interface = attr(x, "interface")
  options = attr(x, "options")
  
  # collapse header
  header <- boxhead |> 
    pull(label) |> 
    as.list() |> 
    collapse_rows()
  
  # collapse body
  text_column_names <- boxhead |> 
    filter(is_text) |> 
    pull(variable)
  
  body <- x |>
    mutate(across(
      all_of(text_column_names), 
      gt::escape_latex
    )) |> 
    mutate(across(
      .cols = where(\(x) !is.character(x)),
      .fns = \(x) format(x, digits = 2L, trim = TRUE)
    )) |>
    collapse_rows() |> 
    str_flatten(collapse = "\n")
  
  # prepend updated colspec from boxhead to interface (warn if already set via `set_interface()`)
  stopifnot(!"colspec" %in% names(interface))
  colspec <- boxhead |> 
    pull(alignment) |> 
    str_flatten()
  interface <- c(list(colspec = colspec), interface)
  
  # format key-value pairs in "interface" and "options"
  interface <- format_key_value_pairs(interface)
  options <- format_key_value_pairs(options)
  
  # choose environment based on type and booktabs
  if (type == "simple") {
    env <- if (booktabs) "booktabs" else "tblr"
  } else if (type == "float") {
    env <- if (booktabs) "talltabs" else "talltblr"
  } else {
    env <- if (booktabs) "longtabs" else "longtblr"
  }
  
  # choose rules based on booktabs
  if (booktabs) {
    toprule <- "\\toprule"
    midrule <- "\\midrule"
    bottomrule <- "\\bottomrule"
  } else {
    toprule <- "\\hline"
    midrule <- "\\hline"
    bottomrule <- "\\hline"
  }
  
  # choose template based on type (simple table does not have options)
  if (type == "simple") {
    template <- "
    \\begin{center}
    \\addtolength{\\leftskip}{-2cm}
    \\addtolength{\\rightskip}{-2cm}
    \\begin{<env>}{
    <interface>
    }
    <toprule>
    <header>
    <midrule>
    <body>
    <bottomrule>
    \\end{<env>}
    \\end{center}
    "
  } else {
    template <- "
    \\begin{center}
    \\addtolength{\\leftskip}{-2cm}
    \\addtolength{\\rightskip}{-2cm}
    \\begin{<env>}[
    <options>
    ]{
    <interface>
    }
    <toprule>
    <header>
    <midrule>
    <body>
    <bottomrule>
    \\end{<env>}
    \\end{center}
    "
  }
  
  # merge variables into template
  out <- stick(template, .null = NULL)

  # assign class "knit_asis"
  knitr::asis_output(out)
}

print.tblr <- function(x) {
  writeLines(tblr_as_latex(x))
}

knit_print.tblr <- function(x, ...) {
  x <- tblr_as_latex(x)
  knitr::knit_print(x, ...)
}
