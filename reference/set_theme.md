# Set theme options

This function allows you to fine-tune the way your `tblr` table object
is rendered as LaTeX.

## Usage

``` r
set_theme(
  x,
  table_indent = NULL,
  table_booktabs = NULL,
  row_group_style = NULL,
  row_group_sep = NULL,
  row_group_indent = NULL,
  row_group_head = NULL,
  row_group_head_alignment = NULL,
  row_group_head_fontstyle = NULL,
  row_group_head_cmidrule = NULL,
  row_group_head_skip_stub = NULL
)
```

## Arguments

- x:

  A `tblr` table object.

- table_indent:

  Slightly indent the table contents? `TRUE` by default. `FALSE` removes
  the indentation by adding "@{}" to both sides of the column
  specification in LaTeX.

- table_booktabs:

  Format the table using `booktabs`? `TRUE` by default. `booktabs` is a
  LaTeX package that modifies the width (thickness) of rules and spacing
  around them.

- row_group_style:

  A preset to style row groups, either `"left"`, `"center"`,
  `"separators"`, or `"panel"`. These presets affect other `row_group_*`
  options but can be overwritten.

- row_group_sep:

  How to separate row groups? Default is `\\addlinespace`. Another
  common choice is `\\midrule`. Specify `character(0)` to remove
  separators.

- row_group_indent:

  Indent rows under a group heading? `TRUE` by default. Indentation is
  achieved by adding an invisible column that is left blank in grouped
  rows.

- row_group_head:

  Should there be headings for row groups? `TRUE` by default.

- row_group_head_alignment:

  The value for the alignment (column specification) of row group heads,
  `"l"` by default.

- row_group_head_fontstyle:

  The font style to use for row group heads, `"\\textit"` (italic) by
  default. Other options include:

  - `"\\textbf"`: boldface

  - `"\\textup"`: standard font style

  - `"\\textsc"`: small caps

  Note that only a single command can be used.

- row_group_head_cmidrule:

  Add a rule below the row group head? `FALSE` by default.

- row_group_head_skip_stub:

  Should the stub (i.e., the first column) be skipped when aligning row
  group heads and drawing rules underneath them? `FALSE` by default.

## Value

A `tblr` table object.

## Examples

``` r
library(dplyr)

df <- tibble(
  continent = c("Europe", "Asia", "Asia", "Europe", "Asia"),
  country = c("Germany","China", "Afghanistan","France","Taiwan"),
  gdp = c(0.17, 0.23, 11.3, 17, 2.4),
  population = c(11.0, 7.3, 123.11, 5, 33)
)

# apply preset "panel" but overwrite to left-align
df |>
  group_by(continent) |>
  tblr() |>
  set_theme(
    row_group_style = "panel",
    row_group_head_alignment = "l"
  )
#> \begin{center}
#>     \begin{booktabs}{
#>         colspec = {lrr}
#>         }
#>         \toprule
#>         country     & gdp                              & population \\ 
#>         \midrule
#>                     & \SetCell[c=2]{l} \textit{Europe} &            \\ 
#>         \cmidrule{2-3}
#>         Germany     & 0.17                             & 11.00      \\ 
#>         France      & 17.00                            & 5.00       \\ 
#>         \addlinespace
#>                     & \SetCell[c=2]{l} \textit{Asia}   &            \\ 
#>         \cmidrule{2-3}
#>         China       & 0.23                             & 7.30       \\ 
#>         Afghanistan & 11.30                            & 123.11     \\ 
#>         Taiwan      & 2.40                             & 33.00      \\ 
#>         \bottomrule
#>     \end{booktabs}
#> \end{center}
```
