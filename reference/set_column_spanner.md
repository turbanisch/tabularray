# Set column spanners

Set column spanner labels. Column spanner labels are labels that appear
above column labels (headers) and span several of them. Call this
function once for each spanner row in the table, starting with the one
at the bottom.

## Usage

``` r
set_column_spanner(x, ...)
```

## Arguments

- x:

  A `tblr` table object.

- ...:

  Expressions for the assignment of column spanner labels. Expressions
  have to be two-sided formulas (i.e., of the form `<LHS> ~ <RHS>`). The
  left-hand side is used to identify selections of columns via
  tidy-select and the right-hand side to specify the column spanner
  label. Note that the column spanner labels you provide are assumed to
  be raw LaTeX strings; characters that have a special meaning in LaTeX
  will not be escaped.

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

tblr(df) |>
  set_column_spanner(!value ~ "Region")
#> \begin{center}
#>     \begin{booktabs}{
#>         colspec = {llr}
#>         }
#>         \toprule
#>         \SetCell[c=2]{c} Region &             &       \\ 
#>         \cmidrule{1-2}
#>         continent               & country     & value \\ 
#>         \midrule
#>         Europe                  & Germany     & 0.17  \\ 
#>         Asia                    & China       & 0.23  \\ 
#>         Asia                    & Afghanistan & 11.30 \\ 
#>         Europe                  & France      & 17.00 \\ 
#>         Asia                    & Taiwan      & 2.40  \\ 
#>         \bottomrule
#>     \end{booktabs}
#> \end{center}
```
