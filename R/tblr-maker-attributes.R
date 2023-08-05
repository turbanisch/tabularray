tblr <- function(df,
                 caption = NULL,
                 source_note = NULL,
                 col_names = NULL,
                 interface = NULL) {
  # escape column names for latex
  if (is_null(col_names)) {
    col_names <- gt::escape_latex(colnames(df))
  }
  
  # generate and append colspec if not specified in options
  if (is_null(interface$colspec)) {
    natural_colspec <- if_else(map_lgl(df, is.numeric), "r", "l") |> str_flatten()
    interface$colspec <- natural_colspec
  }
  
  # find positions of character columns (only those will be escaped)
  character_column_indices <- which(map_lgl(df, is.character))
  
  structure(
    df,
    class = c("tblr", "tbl_df", "tbl", "data.frame"),
    caption = caption,
    source_note = source_note,
    col_names = col_names,
    interface = interface,
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
    collapse_rows()
  
  # evaluate new interface
  interface_vector <- list_simplify(attr(x, "interface"))
  interface <- str_c(
    names(interface_vector), 
    "=", 
    enclose_curly(interface_vector)
  ) |> str_flatten(collapse = ",") |> 
    enclose_curly()
  
  # bind all parts
  tabular <- c(
    str_c("\\begin{booktabs}", interface),
    "\\toprule",
    header,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{booktabs}"
  ) |> str_flatten(collapse = "\n")
  
  caption_line <- if (!is_null(attr(x, "caption"))) str_c("\\caption{", attr(x, "caption"), "}") else NULL
  
  # enclose in table environment
  out <- str_c(
    "\\begin{table}",
    "\\centering",
    caption_line,
    tabular,
    "\\end{table}",
    sep = "\n"
  )
  
  # assign class "knit_asis"
  knitr::asis_output(out)
}

knit_print.tblr <- function(x, ...) {
  x <- tblr_as_latex(x)
  knitr::knit_print(x, ...)
}