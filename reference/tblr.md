# Create a **tblr** table object

This function can be used to initialize a dataframe as a table object of
class `tblr`. Usually, this is just the starting point: Use the pipe and
chain together functions to incrementally add styling and additional
parts (such as notes) to your table.

## Usage

``` r
tblr(df, type = "simple", caption = NULL)
```

## Arguments

- df:

  The `tibble` or `data.frame` to be typeset as a table.

- type:

  The type of LaTeX table environment. This can be either "simple" (the
  default), "float", or "break".

  - "simple" produces an unnumbered and untitled table intended for
    simple tabular matter to appear in the run of the text (see CMOS
    3.51).

  - "float" produces a floating numbered table that accommodates both a
    title and table notes, similar to a `threeparttable` in LaTeX.

  - "break" produces a numbered table that can be broken across pages
    instead of floating. It accommodates both a title and table notes,
    similar to a `longtable` in LaTeX.

- caption:

  Title of the table. Needs to be specified unless `type` is "simple".
  In LaTeX output, this title appears under the "caption" keyword.

## Value

A `data.frame` of class `tblr`.

## Details

`tblr()` preserves the dataframe it is called upon so you can continue
to use functions that operate on dataframes. This function only adds the
class `tblr` to the object and saves some metadata about the table (such
as column types) as attributes. This metadata determines how the table
is rendered at the end of a workflow. If you modify columns after the
call to `tblr()`, you risk breaking this link.

For example, characters that have a special meaning in LaTeX are escaped
only in those columns that were originally text-like (character strings
or factors). If you apply a formatting function to convert a numeric
column to a character vector between your call to `tblr()` and
rendering, you are required to take care of escaping special characters
yourself.

## Examples

``` r
tblr(mtcars)
#> \begin{center}
#>     \begin{booktabs}{
#>         colspec = {rrrrrrrrrrr}
#>         }
#>         \toprule
#>         mpg   & cyl & disp   & hp  & drat & wt   & qsec  & vs & am & gear & carb \\ 
#>         \midrule
#>         21.00 & 6   & 160.00 & 110 & 3.90 & 2.62 & 16.46 & 0  & 1  & 4    & 4    \\ 
#>         21.00 & 6   & 160.00 & 110 & 3.90 & 2.88 & 17.02 & 0  & 1  & 4    & 4    \\ 
#>         22.80 & 4   & 108.00 & 93  & 3.85 & 2.32 & 18.61 & 1  & 1  & 4    & 1    \\ 
#>         21.40 & 6   & 258.00 & 110 & 3.08 & 3.21 & 19.44 & 1  & 0  & 3    & 1    \\ 
#>         18.70 & 8   & 360.00 & 175 & 3.15 & 3.44 & 17.02 & 0  & 0  & 3    & 2    \\ 
#>         18.10 & 6   & 225.00 & 105 & 2.76 & 3.46 & 20.22 & 1  & 0  & 3    & 1    \\ 
#>         14.30 & 8   & 360.00 & 245 & 3.21 & 3.57 & 15.84 & 0  & 0  & 3    & 4    \\ 
#>         24.40 & 4   & 146.70 & 62  & 3.69 & 3.19 & 20.00 & 1  & 0  & 4    & 2    \\ 
#>         22.80 & 4   & 140.80 & 95  & 3.92 & 3.15 & 22.90 & 1  & 0  & 4    & 2    \\ 
#>         19.20 & 6   & 167.60 & 123 & 3.92 & 3.44 & 18.30 & 1  & 0  & 4    & 4    \\ 
#>         17.80 & 6   & 167.60 & 123 & 3.92 & 3.44 & 18.90 & 1  & 0  & 4    & 4    \\ 
#>         16.40 & 8   & 275.80 & 180 & 3.07 & 4.07 & 17.40 & 0  & 0  & 3    & 3    \\ 
#>         17.30 & 8   & 275.80 & 180 & 3.07 & 3.73 & 17.60 & 0  & 0  & 3    & 3    \\ 
#>         15.20 & 8   & 275.80 & 180 & 3.07 & 3.78 & 18.00 & 0  & 0  & 3    & 3    \\ 
#>         10.40 & 8   & 472.00 & 205 & 2.93 & 5.25 & 17.98 & 0  & 0  & 3    & 4    \\ 
#>         10.40 & 8   & 460.00 & 215 & 3.00 & 5.42 & 17.82 & 0  & 0  & 3    & 4    \\ 
#>         14.70 & 8   & 440.00 & 230 & 3.23 & 5.34 & 17.42 & 0  & 0  & 3    & 4    \\ 
#>         32.40 & 4   & 78.70  & 66  & 4.08 & 2.20 & 19.47 & 1  & 1  & 4    & 1    \\ 
#>         30.40 & 4   & 75.70  & 52  & 4.93 & 1.61 & 18.52 & 1  & 1  & 4    & 2    \\ 
#>         33.90 & 4   & 71.10  & 65  & 4.22 & 1.83 & 19.90 & 1  & 1  & 4    & 1    \\ 
#>         21.50 & 4   & 120.10 & 97  & 3.70 & 2.46 & 20.01 & 1  & 0  & 3    & 1    \\ 
#>         15.50 & 8   & 318.00 & 150 & 2.76 & 3.52 & 16.87 & 0  & 0  & 3    & 2    \\ 
#>         15.20 & 8   & 304.00 & 150 & 3.15 & 3.44 & 17.30 & 0  & 0  & 3    & 2    \\ 
#>         13.30 & 8   & 350.00 & 245 & 3.73 & 3.84 & 15.41 & 0  & 0  & 3    & 4    \\ 
#>         19.20 & 8   & 400.00 & 175 & 3.08 & 3.85 & 17.05 & 0  & 0  & 3    & 2    \\ 
#>         27.30 & 4   & 79.00  & 66  & 4.08 & 1.94 & 18.90 & 1  & 1  & 4    & 1    \\ 
#>         26.00 & 4   & 120.30 & 91  & 4.43 & 2.14 & 16.70 & 0  & 1  & 5    & 2    \\ 
#>         30.40 & 4   & 95.10  & 113 & 3.77 & 1.51 & 16.90 & 1  & 1  & 5    & 2    \\ 
#>         15.80 & 8   & 351.00 & 264 & 4.22 & 3.17 & 14.50 & 0  & 1  & 5    & 4    \\ 
#>         19.70 & 6   & 145.00 & 175 & 3.62 & 2.77 & 15.50 & 0  & 1  & 5    & 6    \\ 
#>         15.00 & 8   & 301.00 & 335 & 3.54 & 3.57 & 14.60 & 0  & 1  & 5    & 8    \\ 
#>         21.40 & 4   & 121.00 & 109 & 4.11 & 2.78 & 18.60 & 1  & 1  & 4    & 2    \\ 
#>         \bottomrule
#>     \end{booktabs}
#> \end{center}
```
