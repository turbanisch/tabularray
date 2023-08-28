tblr_as_latex <- function(x) {
  
  stop_if_not_tblr(x)
  
  # add shortcuts to attributes
  type = attr(x, "type")
  boxhead = attr(x, "boxhead")
  interface = attr(x, "interface")
  options = attr(x, "options")
  spanners = attr(x, "spanners")
  theme = attr(x, "theme")
  
  # retrieve group var (df ungrouped by `tblr`)
  group_var <- boxhead |> 
    filter(type == "group") |> 
    pull(variable)
  if (is_empty(group_var)) group_var <- NULL
  is_grouped <- !is_null(group_var)
  
  # collapse spanners
  default_vars <- boxhead$variable[boxhead$type == "default"]
  
  spanners <- spanners |> 
    select(all_of(default_vars)) |> 
    asplit(MARGIN = 1) |> 
    map_chr(\(s) format_colummn_spanners(s, add_indent_col = is_grouped && theme$row_group_indent)) |> 
    str_flatten(collapse = "\n")
  
  if (spanners == "") spanners <- NULL
    
  # collapse header
  header <- boxhead |> 
    filter(type == "default") |> 
    pull(label) |> 
    as.list() |> 
    collapse_rows()
  
  if (is_grouped && theme$row_group_indent) header <- str_c("& ", header)
  
  # collapse body
  text_column_names <- boxhead |> 
    filter(is_text, type == "default") |> 
    pull(variable)
  
  n_default_columns <- sum(boxhead$type == "default")
  
  x_chr <- x |>
    # interpret any text that was in the dataset before tblr() was called as *not* formatted for LaTeX -> escape
    # NA is contagious, defuse by turning into character string ("NA")
    mutate(across(
      all_of(text_column_names), 
      \(s) gt::escape_latex(replace_na(s, "NA"))
    )) |> 
    # convert any remaining non-text columns to text at the end
    mutate(across(
      .cols = where(\(x) !is.character(x)),
      .fns = \(x) format(x, digits = 2L, trim = TRUE)
    ))
  
  if (!is_grouped) {
    body <- x_chr |>
      collapse_row_block()
  } else {
    # use grouping structure to nest
    nested <- x_chr |> nest(.by = all_of(group_var))
    separator_column <- c(rep(theme$row_group_sep, times = nrow(nested) - 1), NA)
    nested <- nested |> add_column(separator_column)
    
    nested_chr <- nested |> 
      mutate(across(1, \(s) format_group_heads(
        s, 
        # un-indent headers only if first column is not skipped (i.e., heading is flush left with table border)
        span_start = 1 + theme$row_group_head_skip_stub + theme$row_group_head_skip_stub * theme$row_group_indent,
        span_end = n_default_columns + theme$row_group_indent,
        colspec = theme$row_group_head_alignment,
        fontstyle = theme$row_group_head_fontstyle,
        cmidrule = theme$row_group_head_cmidrule
      ))) |> 
      mutate(data = map_chr(data, \(x) collapse_row_block(x, add_indent_col = theme$row_group_indent)))
    
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
  if (is_grouped && theme$row_group_indent) colspec <- str_c("l", colspec)
  if (!theme$table_indent) colspec <- str_c("@{}", colspec, "@{}")
  interface <- c(list(colspec = colspec), interface)
  
  # format key-value pairs in "interface" and "options"
  interface <- format_key_value_pairs(interface)
  options <- format_key_value_pairs(options)
  
  # choose environment based on type and booktabs
  if (type == "simple") {
    env <- if (theme$table_booktabs) "booktabs" else "tblr"
  } else if (type == "float") {
    env <- if (theme$table_booktabs) "talltabs" else "talltblr"
  } else {
    env <- if (theme$table_booktabs) "longtabs" else "longtblr"
  }
  
  # choose rules based on booktabs
  if (theme$table_booktabs) {
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
    <spanners>
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
    <spanners>
    <header>
    <midrule>
    <body>
    <bottomrule>
    \\end{<env>}
    \\end{center}
    "
  }
  
  # merge variables into template (missing components result in empty lines)
  out <- stick(template, .null = NULL) |> 
    remove_empty_lines()
  
  # assign class "knit_asis"
  knitr::asis_output(out)
}