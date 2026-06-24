#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @importFrom glue glue
## usethis namespace: end
NULL

# Column names referenced via dplyr's non-standard evaluation inside
# tblr_as_latex(); declared here to satisfy R CMD check.
utils::globalVariables(c("variable", "label", "alignment", "data"))
