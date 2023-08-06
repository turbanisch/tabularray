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
  
  # enclose values in {} and flatten; empty character vector returns ""
  named_vec <- map(
    named_list,
    \(x) x |> enclose_curly() |> str_flatten()
  ) |> list_simplify()
  
  # add "= value" only if there is a value
  add_equal_sign_if <- function(condition) {
    if_else(condition, " = ", "")
  }
  
  str_c(
    names(named_vec),
    add_equal_sign_if(str_length(named_vec) > 0L),
    named_vec
  ) |> str_flatten(collapse = ",\n")
}
