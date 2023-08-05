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