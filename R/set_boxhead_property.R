# multiple implementations below
# different interface (with dots or single argument `cols`; as named list or formula in dots)
# different functionality (tidy-select or not)

# single "column" argument with tidy-select -------------------------------

set_boxhead_property <- function(df, cols, property, replacement) {
  # find position/names of variables selected via tidy-select
  expr <- rlang::enquo(cols)
  target_col_positions <- tidyselect::eval_select(expr, data = df)
  target_col_names <- names(target_col_positions)
  
  # replace value in the property-column in rows corresponding to those variables
  boxhead <- attr(df, "boxhead")
  boxhead[boxhead$variable %in% target_col_names, property] <- replacement
  
  attr(df, "boxhead") <- boxhead
  return(df)
}

# columns + value as named list in dots argument (no tidy-select) ----------

set_boxhead_property <- function(df, property, ...) {
  
  kwargs <- rlang::list2(...)
  
  target_col_names <- names(kwargs)
  replacement <- unname(kwargs)
  
  boxhead <- attr(df, "boxhead")
  
  
  # replace value in the property-column in rows corresponding to those variables
  for (i in seq_along(kwargs)) {
    boxhead[boxhead$variable %in% target_col_names[i], property] <- replacement[i]
  }
  
  attr(df, "boxhead") <- boxhead
  return(df)
}

set_alignment <- function(df, ...) {
  set_boxhead_property(df = df, property = "alignment", ...)
}


# columns + value as formulas in dots list (with tidy-select) -------------

set_boxhead_property <- function(df, property, ...) {
  
  kwargs <- rlang::list2(...)
  boxhead <- attr(df, "boxhead")
  
  for (i in seq_along(kwargs)) {
    
    expr <-  rlang::f_lhs(kwargs[[i]])
    target_col_positions <- tidyselect::eval_select(expr, data = df)
    target_col_names <- names(target_col_positions)
    
    replacement <- kwargs[[i]] |> rlang::f_rhs()
    
    # replace value in the property-column in rows corresponding to those variables
    boxhead[boxhead$variable %in% target_col_names, property] <- replacement
  }
  
  attr(df, "boxhead") <- boxhead
  return(df)
}



# columns + value as named list or formula in dots argument (with  --------

set_boxhead_property <- function(df, property, ...) {
  
  kwargs <- rlang::list2(...)
  boxhead <- attr(df, "boxhead")
  
  for (i in seq_along(kwargs)) {
    
    if (rlang::is_formula(kwargs[[i]])) {
      expr <-  rlang::f_lhs(kwargs[[i]])
      target_col_positions <- tidyselect::eval_select(expr, data = df)
      target_col_names <- names(target_col_positions)
      
      replacement <- kwargs[[i]] |> rlang::f_rhs()
    } else {
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

set_alignment <- function(df, cols, align) {
  set_boxhead_property(df = df, cols = {{cols}}, property = "alignment", replacement = align)
}


# apply combined ----------------------------------------------------------

tblr(df) |> set_alignment(value = "X", starts_with("c") ~ "c")