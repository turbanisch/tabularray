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
#   rows = {7mm}
# }

# assign key-value pairs as elements of list stored in attribute 

# main function -----------------------------------------------------------

set_list_attribute <- function(x, attribute_name, names_prefix = NULL, ...) {
  
  stop_if_not_tblr(x)
  dots_list <- rlang::list2(...)
  
  # prefix all names in the list with a common string, e.g. for remark{...}
  if (!is_null(names_prefix)) {
    names(dots_list) <- str_c(names_prefix, "{", names(dots_list), "}")
  }
  
  attr(x, attribute_name) <- purrr::list_assign(
    attr(x, attribute_name),
    !!!dots_list
  )
  
  return(x)
}


# wrapper functions to set list attributes --------------------------------

set_interface <- function(x, ...) {
  set_list_attribute(x, "interface", ...)
}

set_options <- function(x, ...) {
  set_list_attribute(x, "options", ...)
}

set_source_notes <- function(x, ...) {
  set_list_attribute(x, "options", names_prefix = "remark", ...)
}