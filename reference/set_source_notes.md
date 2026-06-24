# Add source notes

Add table notes to your table. Only `tblr` table objects of type "float"
or "break" support table notes.

## Usage

``` r
set_source_notes(x, ...)
```

## Arguments

- x:

  A `tblr` table object of type "float" or "break".

- ...:

  Named arguments to add notes under a table. Names provide a label for
  each note; the argument value is the actual note itself.

## Value

A `tblr` table object.

## Examples

``` r
library(dplyr)
df <- tibble(
  continent = c("Europe", "Asia", "Asia", "Europe", "Asia"),
  country = c("Germany","China", "Afghanistan","France","Taiwan"),
  value = c(0.17, 0.23, 11.3, 17, 2.4)
)

x <- tblr(df, type = "float", caption = "A table with notes")

x |> set_source_notes(
  Source = "Built-in R dataset",
  Note = paste("This dataset comes with every R installation and is",
               "available without explicitly loading it.")
)
#> \begin{center}
#>     \begin{talltabs}[
#>         caption = {A table with notes},
#>         remark{Source} = {Built-in R dataset},
#>         remark{Note} = {This dataset comes with every R installation and is available without explicitly loading it.}
#>         ]{
#>         colspec = {llr}
#>         }
#>         \toprule
#>         continent & country     & value \\ 
#>         \midrule
#>         Europe    & Germany     & 0.17  \\ 
#>         Asia      & China       & 0.23  \\ 
#>         Asia      & Afghanistan & 11.30 \\ 
#>         Europe    & France      & 17.00 \\ 
#>         Asia      & Taiwan      & 2.40  \\ 
#>         \bottomrule
#>     \end{talltabs}
#> \end{center}
```
