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

remove_empty_lines <- function(s) {
  str_replace_all(s, "\\n\\n", "\\\n")
}

# paste character vectors stored in a list (e.g., a dataframe), output is character vector
collapse_rows <- function(l) {
  do.call(\(...) str_c(..., sep = " & "), l) |>
    append_line_break()
}

stop_if_not_tblr <- function(x) {
  stopifnot("tblr" %in% class(x))
}

# collapse rows and flatten into a single character vector
collapse_row_block <- function(df, add_indent_col = FALSE) {
  row_vector <- collapse_rows(df)
  if (add_indent_col) row_vector <- str_c("& ", row_vector)
  str_flatten(row_vector, collapse = "\n")
}

format_group_heads <- function(
    s,
    span_start,
    span_end,
    colspec = "l",
    fontstyle = "\\textbf",
    cmidrule = FALSE
) {

  n_spanned_columns <- span_end - span_start + 1
  prefix <- str_flatten(rep("&", times = span_start - 1),
                        collapse = " ")
  prefix <- if (is_empty(prefix)) NULL else str_c(prefix, " ")
  suffix <- if (cmidrule) stick("\\cmidrule{<span_start>-<span_end>}") else NULL

  stick("<prefix>\\SetCell[c=<n_spanned_columns>]{<colspec>} <fontstyle>{<s>}<line_break>
        <suffix>",
        .null = NULL) |>
    # remove trailing line break if suffix is empty
    str_remove("\\n$")
}

format_colummn_spanners <- function(spanner, add_indent_col = FALSE) {

  # calculate number of spanned columns
  occurences <- table(spanner)
  n_span <- as.vector(occurences[spanner])

  # convert spanned columns to NA (should be empty cells in LaTeX)
  first <- match(unique(spanner), spanner, incomparables = NA)
  first <- first[!is.na(first)]
  not_first <- setdiff(seq_along(spanner), first)
  spanner[not_first] <- NA

  # calculate span range (from - to) for cmidrules
  span_start <- first + add_indent_col
  span_end <- first + n_span[first] - 1 + add_indent_col
  span_range <- if_else(
    span_start == span_end,
    as.character(span_start),
    str_c(span_start, "-", span_end)
  )

  # format
  trim <- str_c(
    if_else(span_start == lag(span_end) + 1, "l", "", missing = ""),
    if_else(span_end + 1 == lead(span_start), "r", "", missing = "")
  )

  cmidrule_row <- stick("\\cmidrule[<trim>]{<span_range>}") |>
    # remove empty options
    str_remove("\\[\\]") |>
    str_flatten(collapse = " ")

  spanner_row <- if_else(
    is.na(spanner),
    "",
    stick("\\SetCell[c=<n_span>]{c} <spanner>")
  ) |>
    str_flatten(collapse = " & ") |>
    str_trim() |>
    append_line_break()

  if (add_indent_col) spanner_row <- str_c("& ", spanner_row)

  str_c(spanner_row, cmidrule_row, sep = "\n")
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

# find maximum number of characters from both columns and colnames to specify padding
max_nchar_per_col <- function(column_labels, df) {

  label_lengths <- str_length(column_labels)
  body_lengths <- as.list(df) |>
    map_int(\(x) max(str_length(x)))

  pmax(label_lengths, body_lengths)
}
