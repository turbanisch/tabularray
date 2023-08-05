# helper functions
line_break <- " \\\\ "

enclose_curly <- function(s) {
  str_c("{", s, "}")
}

append_line_break <- function(s) {
  str_c(s, line_break)
}

collapse_rows <- function(l) {
  do.call(\(...) str_c(..., sep = " & "), l) |> 
    append_line_break()
}

escape_line_breaks <- function(s) {
  if_else(
    str_detect(s, "\\n"),
    s |> str_replace_all("\\n", " \\\\\\\\ ") |> enclose_curly(),
    s
  )
}

tblr <- function(df,
                 caption = NULL,
                 source_note = NULL,
                 colspec = NULL) {
  x <- list(data = df,
            caption = caption,
            source_note = source_note)
  
  class(x) <- c("tblr", "list")
  return(x)
}

tblr_as_latex <- function(tblr) {
  
  df <- tblr$data
  caption <- tblr$caption
  
  # generate colspec
  colspec <- if_else(map_lgl(df, is.numeric), "r", "l") |> 
    str_flatten() |> 
    enclose_curly()
  
  # collapse header
  header <- colnames(df) |> 
    escape_line_breaks() |> 
    as.list() |> 
    collapse_rows()
  
  # collapse body
  body <- df |> 
    mutate(across(where(is_character), escape_line_breaks)) |> 
    collapse_rows()
  
  # bind all parts
  tabular <- c(
    str_c("\\begin{booktabs}", colspec),
    "\\toprule",
    header,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{booktabs}"
  ) |> str_flatten(collapse = "\n")
  
  caption_command <- if (!is_null(caption)) str_c("\\caption{", caption, "}") else NULL
  
  # enclose in table environment
  out <- str_c(
    "\\begin{table}",
    "\\centering",
    caption_command,
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