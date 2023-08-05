line_break <- " \\\\ "

enclose_curly <- function(s) {
  str_c("{", s, "}")
}

enclose_square <- function(s) {
  str_c("[", s, "]")
}

append_line_break <- function(s) {
  str_c(s, line_break)
}

collapse_rows <- function(l) {
  do.call(\(...) str_c(..., sep = " & "), l) |> 
    append_line_break()
}

stick <- function(..., .open = "<", .close = ">", .envir = parent.frame()) {
  glue::glue(..., .open = .open, .close = .close, .envir = .envir, .null = NULL)
}

format_key_value_pairs <- function(named_list) {
  
  if (is_empty(named_list)) return(NULL)
  
  named_vec <- list_simplify(named_list)
  
  str_c(
    names(named_vec), 
    " = ", 
    enclose_curly(named_vec)
  ) |> str_flatten(collapse = ",\n")
}