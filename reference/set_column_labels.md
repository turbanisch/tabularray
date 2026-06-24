# Set column labels

Define column labels (headers). By default, column labels are derived
from the column names of the underlying dataframe. However, unlike those
column names, column labels do not have to be unique. Note that column
labels are only used for rendering the table; other functions still use
the original column names to refer to columns of the underlying
dataframe.

## Usage

``` r
set_column_labels(x, ...)
```

## Arguments

- x:

  A `tblr` table object.

- ...:

  Expressions for the assignment of column labels. Expressions can be
  named arguments of the form `<column name> = <label>` or make use of
  tidy-select. When using tidy-select, expressions have to be two-sided
  formulas (i.e., of the form `<LHS> ~ <RHS>`). In this case, the
  left-hand side is used to identify selections of columns and the
  right-hand side to specify the column label. Note that the column
  labels you specify are assumed to be raw LaTeX strings; characters
  that have a special meaning in LaTeX will not be escaped.

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
  set_column_labels(value = "\\textbf{Value}", ends_with("y") ~ "COUNTRY")
#> \begin{center}
#>     \begin{booktabs}{
#>         colspec = {llr}
#>         }
#>         \toprule
#>         continent & COUNTRY     & \textbf{Value} \\ 
#>         \midrule
#>         Europe    & Germany     & 0.17           \\ 
#>         Asia      & China       & 0.23           \\ 
#>         Asia      & Afghanistan & 11.30          \\ 
#>         Europe    & France      & 17.00          \\ 
#>         Asia      & Taiwan      & 2.40           \\ 
#>         \bottomrule
#>     \end{booktabs}
#> \end{center}
```
