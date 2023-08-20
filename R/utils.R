line_break <- " \\\\ "

enclose_curly <- function(s) {
  str_c("{", s, "}")
}

enclose_square <- function(s) {
  str_c("[", s, "]")
}

str_prepend <- function(prefix, s) {
  str_c(prefix, s)
}

append_line_break <- function(s) {
  str_c(s, line_break)
}

# paste character vectors stored in a list (e.g., a dataframe), output is character vector
collapse_rows <- function(l) {
  do.call(\(...) str_c(..., sep = " & "), l) |> 
    append_line_break()
}

# collapse rows and flatten into a single character vector
collapse_row_block <- function(df, add_indent_col = FALSE) {
  row_vector <- collapse_rows(df)
  if (add_indent_col) row_vector <- str_c("& ", row_vector)
  str_flatten(row_vector, collapse = "\n")
}

format_group_heads <- function(s, n_spanned_columns, colspec = "l") {
  stick("\\SetCell[c=<n_spanned_columns>]{<colspec>} \\textbf{<s>}<line_break>")
}

stick <- function(..., .open = "<", .close = ">", .envir = parent.frame()) {
  glue::glue(..., .open = .open, .close = .close, .envir = .envir)
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
