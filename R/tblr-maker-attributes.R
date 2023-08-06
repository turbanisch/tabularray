tblr <- function(df,
                 type = "float",
                 booktabs = TRUE,
                 caption = NULL,
                 source_notes = NULL,
                 col_names = NULL,
                 interface = list(),
                 options = list()) {
  # sanity checks
  type <- match.arg(type, c("draft", "float", "break"))
  if (type != "draft") stopifnot(!is_null(caption) || !is_null(options$caption)) # types other than draft need caption
  
  # escape column names for latex
  if (is_null(col_names)) {
    col_names <- gt::escape_latex(colnames(df))
  }
  
  # generate and append colspec if not specified in interface
  if (is_null(interface$colspec)) {
    natural_colspec <- if_else(map_lgl(df, is.numeric), "r", "l") |> str_flatten()
    interface$colspec <- natural_colspec
  }
  
  # replace options with shortcut arguments
  if (!is_null(caption)) options$caption <- caption
  
  if (!is_null(source_notes)) {
    replacements <- set_names(
      source_notes,
      str_c("remark", enclose_curly(names(source_notes)))
    )
    options <- purrr::list_assign(options, !!!replacements)
  }
  
  
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
  if (attr(x, "type") == "draft") {
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
  
  # choose template based on type (draft table does not have options)
  if (attr(x, "type") == "draft") {
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
  out <- stick(template)

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
