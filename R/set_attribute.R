# assign key-value pairs as elements of list stored in attribute

# main function -----------------------------------------------------------

set_attribute <- function(x, attribute_name, ...) {
  dots_list <- rlang::list2(...)
  
  attr(x, attribute_name) <- purrr::list_assign(
    attr(x, attribute_name),
    !!!dots_list
  )
  
  return(x)
}


# wrapper functions -------------------------------------------------------

set_interface <- function(x, ...) {
  set_attribute(x, "interface", ...)
}

set_options <- function(x, ...) {
  set_attribute(x, "options", ...)
}

# new setter functions
# set_col_names
# 
# set_source_notes:
#   if (!is_null(source_notes)) {
#     replacements <- set_names(
#       source_notes,
#       str_c("remark", enclose_curly(names(source_notes)))
#     )
#     options <- purrr::list_assign(options, !!!replacements)
#   }



# example -----------------------------------------------------------------


# x |>
#   set_interface(
#     hlines = character(0),
#     vlines = c("1,3,5", "dashed"),
#     rows = "7mm"
#   )

# {
#   hlines, 
#   vlines = {1, 3, 5}{dashed},
#   rows = {7mm}, columns = {15mm,c}
# }
