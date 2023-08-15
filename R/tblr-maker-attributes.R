#' documentation
#' type: The type of LaTeX table environment. This can be either "simple" (the default), "float", or "break". The corresponding LaTeX environments defined in the **tabularray** package are "tblr", "talltblr", and "longtblr". "simple" produces an unnumbered and untitled table intended for simple tabular matter to appear in the run of the text (see CMOS 3.51). "float" produces a floating numbered table that accommodates both a title and table notes, similar to a "threeparttable" in LaTeX. "break" produces a numbered table that can be broken across pages instead of floating. It accommodates both a title and table notes, similar to a "longtable" in LaTeX. Could use table: simple + float + break ~ tabularray environment + traditional LaTeX environment + Description
#' booktabs: Should tables be formatted with booktabs styling? This setting modifies the thickness of rules (horizontal borders) and spacing between lines. If `TRUE` (the default), the booktabs versions of the LaTeX table environments corresponding to "type" will be used. These are "booktabs", "talltabs", and "longtabs". Could use table: plain + booktabs ~ simple + float + break
#' caption: Title of the table. Needs to be specified unless `type` is "simple". In LaTeX output, this title appears under the "caption" keyword.

tblr <- function(df,
                 type = "simple",
                 booktabs = TRUE,
                 caption = NULL) {
  # sanity checks
  type <- match.arg(type, c("simple", "float", "break"))
  if (type != "simple") stopifnot(!is_null(caption))
  
  # initialize "interface" and "options" 
  interface <- list()
  options <- list()
  
  # escape column names for latex
  col_names <- gt::escape_latex(colnames(df))
  
  # generate interface list and populate with colspec
  natural_colspec <- if_else(map_lgl(df, is.numeric), "r", "l") |> str_flatten()
  interface$colspec = natural_colspec
  
  # replace options with shortcut arguments
  if (!is_null(caption)) options$caption <- caption
  
  # find positions of text-like columns (only those will be escaped)
  df <- df |> mutate(across(where(is.factor), as.character))
  character_column_indices <- which(map_lgl(df, is.character))
  
  structure(
    df,
    class = c("tblr", "tbl_df", "tbl", "data.frame"),
    type = type,
    booktabs = booktabs,
    col_names = col_names,
    interface = interface,
    options = options,
    character_column_indices = character_column_indices
  )
}

tblr_as_latex <- function(x) {
  
  # collapse header
  header <- attr(x, "col_names") |> 
    as.list() |> 
    collapse_rows()
  
  # collapse body
  body <- x |>
    mutate(across(attr(x, "character_column_indices"), gt::escape_latex)) |> 
    mutate(across(
      .cols = where(\(x) !is.character(x)),
      .fns = \(x) format(x, trim = TRUE)
    )) |>
    collapse_rows() |> 
    str_flatten(collapse = "\n")
  
  # format list arguments
  interface <- format_key_value_pairs(attr(x, "interface"))
  options <- format_key_value_pairs(attr(x, "options"))
  
  # choose environment based on type and booktabs
  if (attr(x, "type") == "simple") {
    env <- if (attr(x, "booktabs")) "booktabs" else "tblr"
  } else if (attr(x, "type") == "float") {
    env <- if (attr(x, "booktabs")) "talltabs" else "talltblr"
  } else {
    env <- if (attr(x, "booktabs")) "longtabs" else "longtblr"
  }
  
  # choose rules based on booktabs
  if (attr(x, "booktabs")) {
    toprule <- "\\toprule"
    midrule <- "\\midrule"
    bottomrule <- "\\bottomrule"
  } else {
    toprule <- "\\hline"
    midrule <- "\\hline"
    bottomrule <- "\\hline"
  }
  
  # choose template based on type (simple table does not have options)
  if (attr(x, "type") == "simple") {
    template <- "
    \\begin{center}
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
