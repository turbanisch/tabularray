# Shared fixtures for tests --------------------------------------------------
#
# `countries` mirrors the data frame used throughout the roxygen @examples;
# `tatooine` reproduces the README example (including the special-character
# entry "C$PO"). Keeping them here avoids repeating the literals in every test.

countries <- function() {
  tibble::tibble(
    continent = c("Europe", "Asia", "Asia", "Europe", "Asia"),
    country = c("Germany", "China", "Afghanistan", "France", "Taiwan"),
    value = c(0.17, 0.23, 11.3, 17, 2.4)
  )
}

countries2 <- function() {
  tibble::tibble(
    continent = c("Europe", "Asia", "Asia", "Europe", "Asia"),
    country = c("Germany", "China", "Afghanistan", "France", "Taiwan"),
    gdp = c(0.17, 0.23, 11.3, 17, 2.4),
    population = c(11, 7.3, 123.11, 5, 33)
  )
}

tatooine <- function() {
  df <- dplyr::arrange(
    dplyr::select(
      dplyr::filter(dplyr::starwars, homeworld == "Tatooine"),
      name, height, mass, sex, birth_year
    ),
    dplyr::desc(birth_year)
  )
  df[1, 1] <- "C$PO"
  df
}

# render helper: the LaTeX string (drops the knit_asis class for plain compare)
as_latex <- function(x) as.character(tblr_as_latex(x))
