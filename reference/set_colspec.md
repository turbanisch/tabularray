# Set column specifications

Set the column specification (`colspec`) of one or more columns. The
column specification controls alignment and, more generally, the
`tabularray` column type. By default, columns are left-aligned ("l"), or
right-aligned ("r") if they are numeric.

## Usage

``` r
set_colspec(x, ...)
```

## Arguments

- x:

  A `tblr` table object.

- ...:

  Expressions for the assignment of column specifications. Expressions
  can be named arguments of the form `<column name> = <colspec>` or make
  use of tidy-select. When using tidy-select, expressions have to be
  two-sided formulas (i.e., of the form `<LHS> ~ <RHS>`). In this case,
  the left-hand side is used to identify selections of columns and the
  right-hand side to specify the column specification. Note that the
  column specifications you supply are assumed to be raw LaTeX strings;
  characters that have a special meaning in LaTeX will not be escaped.

## Value

A `tblr` table object.

## Details

Column specifications can be more than just left ("l"), center ("c"),
and right ("r"). In `tabularray`, all column types are derived from the
column type `Q`. Useful shorthands include `X` (to evenly distribute
columns given a fixed table width) and `S` (to format columns using
`siunitx`, e.g. `"S[table-format=2.3]"`).

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
df <- tibble(
  continent = c("Europe", "Asia", "Asia", "Europe", "Asia"),
  country = c("Germany","China", "Afghanistan","France","Taiwan"),
  value = c(0.17, 0.23, 11.3, 17, 2.4)
)

tblr(df) |>
  set_colspec(value = "X", starts_with("c") ~ "c")
#> \begin{center}
#>     \begin{booktabs}{
#>         colspec = {ccX}
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
#>     \end{booktabs}
#> \end{center}
```
