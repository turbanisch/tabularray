# example -----------------------------------------------------------------

# df <- tibble(
#   continent = c("Europe", "Asia", "Asia", "Europe", "Asia"),
#   country = c("Germany","China", "Afghanistan","France","Taiwan"),
#   value = c(0.17, 0.23, 11.3, 17, 2.4)
# )
# 
# tblr(df) |> 
#   set_alignment(value = "X", starts_with("c") ~ "c")
# 
# tblr(df) |> 
#   set_column_labels(value = "\\textbf{Value}", ends_with("y") ~ "COUNTRY")


# main function -----------------------------------------------------------

set_boxhead_property <- function(x, property, ...) {
  
  stop_if_not_tblr(x)
  kwargs <- rlang::list2(...)
  boxhead <- attr(x, "boxhead")
  
  for (i in seq_along(kwargs)) {
    
    if (rlang::is_formula(kwargs[[i]])) {
      # interpret LHS as tidy-select if element is a formula
      # no need to use `enquo()`, LHS is already a symbol
      expr <-  rlang::f_lhs(kwargs[[i]])
      target_col_positions <- tidyselect::eval_select(expr, data = x)
      target_col_names <- names(target_col_positions)
      replacement <- rlang::f_rhs(kwargs[[i]])
    } else {
      # otherwise interpret as named list element
      # note that names are a property of the entire list, not of an individual element
      target_col_names <- names(kwargs)[i]
      replacement <- kwargs[[i]]
    }
    
    # replace value in the property-column in rows corresponding to those variables
    boxhead[boxhead$variable %in% target_col_names, property] <- replacement
  }
  
  attr(x, "boxhead") <- boxhead
  return(x)
}


# wrapper functions -------------------------------------------------------

set_alignment <- function(x, ...) {
  set_boxhead_property(x = x, property = "alignment", ...)
}

set_column_labels <- function(x, ...) {
  set_boxhead_property(x = x, property = "label", ...)
}