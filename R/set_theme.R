
# examples ----------------------------------------------------------------

x |> set_theme(row_group_style = "center") |> unclass() |> attr("theme")
x |> set_theme(row_group_head_fontstyle = "\\textit")

# main function -----------------------------------------------------------

set_theme <- function(
  x,
  table_indent = NULL,
  table_booktabs = NULL,
  row_group_sep = NULL,
  row_group_style = NULL,
  row_group_indent = NULL,
  row_group_head_alignment = NULL,
  row_group_head_fontstyle = NULL
) {
  # turn argument name-value pairs into list (excluding data argument and NULL arguments)
  arg_names <- setdiff(
    names(formals(set_theme)),
    "x"
  )
  arg_vals <- mget(arg_names) |> discard(is_null)
  
  # expand meta theme options (more specific options survive if set)
  arg_vals <- expand_meta_theme_options(arg_vals)
  
  # overwrite theme options in tblr object
  attr(x, "theme") <- purrr::list_assign(
    attr(x, "theme"),
    !!!arg_vals
  )
  
  return(x)
}


# helper functions --------------------------------------------------------

expand_meta_theme_options <- function(arg_vals) {
  
  if (!"row_group_style" %in% names(arg_vals)) return(arg_vals)
  
  if (arg_vals$row_group_style == "left") {
    # left
    replacements <- list(
      row_group_indent = TRUE,
      row_group_head_alignment = "l",
      row_group_head_fontstyle = "\\textbf"
    )
  } else {
    # center
    replacements <- list(
      row_group_indent = FALSE,
      row_group_head_alignment = "c",
      row_group_head_fontstyle = "\\textit"
    )
  }
  
  # add back non-meta theme options (these take precedence over meta options)
  other_arg_vals <- arg_vals |> discard_at("row_group_style")
  out <- purrr::list_assign(
    replacements,
    !!!other_arg_vals
  )
  out
}
