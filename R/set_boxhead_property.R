# example -----------------------------------------------------------------

# df <- tibble(
#   continent = c("Europe", "Asia", "Asia", "Europe", "Asia"),
#   country = c("Germany","China", "Afghanistan","France","Taiwan"),
#   value = c(0.17, 0.23, 11.3, 17, 2.4)
# )
# 
# tblr(df) |> 
#   set_alignment(value = "X", starts_with("c") ~ "c")


# main function -----------------------------------------------------------

set_boxhead_property <- function(df, property, ...) {
  
  kwargs <- rlang::list2(...)
  boxhead <- attr(df, "boxhead")
  
  for (i in seq_along(kwargs)) {
    
    if (rlang::is_formula(kwargs[[i]])) {
      # interpret LHS as tidy-select if element is a formula
      # no need to use `enquo()`, LHS is already a symbol
      expr <-  rlang::f_lhs(kwargs[[i]])
      target_col_positions <- tidyselect::eval_select(expr, data = df)
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
  
  attr(df, "boxhead") <- boxhead
  return(df)
}


# wrapper functions -------------------------------------------------------

set_alignment <- function(df, ...) {
  set_boxhead_property(df = df, property = "alignment", ...)
}
