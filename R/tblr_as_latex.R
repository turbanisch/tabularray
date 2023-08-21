tblr_as_latex <- function(x) {
  
  # add shortcuts to attributes
  type = attr(x, "type")
  booktabs = attr(x, "booktabs")
  boxhead = attr(x, "boxhead")
  interface = attr(x, "interface")
  options = attr(x, "options")
  
  # set defaults for theme options: hardcoded for now
  row_group_style <- "left"
  remove_global_indent <- FALSE
  
  if (row_group_style == "left") {
    # left: group headings left-aligned, ordinary rows indented by an empty column
    group_head_alignment <- "l"
    indent_ordinary_rows <- TRUE
    remove_global_indent <- TRUE
    row_block_sep <- "\\addlinespace" # could be `midrule` (defined below) or NA (=none)
  } else {
    # center: group headings centered, ordinary rows not indented
    group_head_alignment <- "c"
    indent_ordinary_rows <- FALSE
    row_block_sep <- NA # no separator
  }
  
  # collapse header
  header <- boxhead |> 
    filter(type == "default") |> 
    pull(label) |> 
    as.list() |> 
    collapse_rows()
  
  if (indent_ordinary_rows) header <- str_c("& ", header)
  
  # collapse body
  text_column_names <- boxhead |> 
    filter(is_text, type == "default") |> 
    pull(variable)
  
  group_column_name <- boxhead |> 
    filter(type == "group") |> 
    pull(variable)
  
  n_default_columns <- sum(boxhead$type == "default")
  
  x_chr <- x |>
    # interpret any text that was in the dataset before tblr() was called as *not* formatted for LaTeX -> escape
    mutate(across(
      all_of(text_column_names), 
      gt::escape_latex
    )) |> 
    # convert any remaining non-text columns to text at the end
    mutate(across(
      .cols = where(\(x) !is.character(x)),
      .fns = \(x) format(x, digits = 2L, trim = TRUE)
    ))
  
  if (is_empty(group_column_name)) {
    body <- x_chr |>
      collapse_row_block()
  } else {
    separator_column <- c(rep(row_block_sep, times = nrow(nested) - 1), NA)
    
    nested <- x_chr |> 
      nest(.by = all_of(group_column_name)) |> 
      add_column(separator_column)
    
    nested_chr <- nested |> 
      mutate(across(1, \(s) format_group_heads(
        s, 
        n_spanned_columns = (n_default_columns + indent_ordinary_rows), 
        colspec = group_head_alignment
      ))) |> 
      mutate(data = map_chr(data, \(x) collapse_row_block(x, add_indent_col = indent_ordinary_rows)))
    
    body <- nested_chr |> 
      as.list() |>
      list_transpose() |>
      list_c() |>
      str_flatten(collapse = "\n", na.rm = TRUE) 
  }
  
  # prepend updated colspec from boxhead to interface (warn if already set via `set_interface()`)
  stopifnot(!"colspec" %in% names(interface))
  colspec <- boxhead |> 
    filter(type == "default") |> 
    pull(alignment) |> 
    str_flatten()
  if (indent_ordinary_rows) colspec <- str_c("l", colspec)
  if (remove_global_indent) colspec <- str_c("@{}", colspec, "@{}")
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