# call function multiple times for multiple levels of spanners
# define spanners from bottom to top

set_column_spanner <- function(x, ...) {
  
  stop_if_not_tblr(x)
  spanners <- attr(x, "spanners")
  kwargs <- rlang::list2(...)
  stopifnot(all(map_lgl(kwargs, rlang::is_formula)))
  
  # initialize new row with NAs
  new_spanner_row <- spanners[1,]
  new_spanner_row[1,] <- NA
  
  # replace NA with LHS, location specified by tidy-select
  for (i in seq_along(kwargs)) {
    expr <- rlang::f_lhs(kwargs[[i]])
    new_spanner_row <- new_spanner_row |> 
      mutate(across(
        {{ expr }},
        \(x) replace_na(x, rlang::f_rhs(kwargs[[i]]))
      ))
  }
  
  # save new column spanner row at top of df
  attr(x, "spanners") <- bind_rows(
    new_spanner_row,
    spanners
  )
  return(x)
}