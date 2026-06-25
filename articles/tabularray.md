# Get started with tabularray

``` r

library(tabularray)
library(dplyr)

# helper to display the generated LaTeX in this vignette
show_latex <- function(x) cat(as.character(tblr_as_latex(x)))
```

`tabularray` turns a data frame into LaTeX markup for the
[tabularray](https://ctan.org/pkg/tabularray) LaTeX package. Inspired by
[gt](https://gt.rstudio.com), you build a table by piping a data frame
into
[`tblr()`](https://turbanisch.github.io/tabularray/reference/tblr.md)
and then chaining `set_*()` functions to add styling, before rendering
with
[`tblr_as_latex()`](https://turbanisch.github.io/tabularray/reference/tblr_as_latex.md).

## A first table

[`tblr()`](https://turbanisch.github.io/tabularray/reference/tblr.md)
initializes a data frame as a table object. Printing it (or calling
[`tblr_as_latex()`](https://turbanisch.github.io/tabularray/reference/tblr_as_latex.md))
produces LaTeX:

``` r

df <- tibble(
  continent = c("Europe", "Asia", "Asia"),
  country   = c("Germany", "China", "Afghanistan"),
  value     = c(0.17, 0.23, 11.3)
)

df |> tblr() |> show_latex()
```

    \begin{center}
        \begin{booktabs}{
            colspec = {llr}
            }
            \toprule
            continent & country     & value \\ 
            \midrule
            Europe    & Germany     & 0.17  \\ 
            Asia      & China       & 0.23  \\ 
            Asia      & Afghanistan & 11.30 \\ 
            \bottomrule
        \end{booktabs}
    \end{center}

Numeric columns are right-aligned, text columns left-aligned. Column
labels default to the column names.

## Formatting

`tabularray` does not try to be a formatting toolkit. For currency,
percentages, thousands separators, significant digits, custom date
formats, or any other per-column formatting, format the column before
calling
[`tblr()`](https://turbanisch.github.io/tabularray/reference/tblr.md) —
for example with [scales](https://scales.r-lib.org),
[`base::format()`](https://rdrr.io/r/base/format.html),
[`base::formatC()`](https://rdrr.io/r/base/formatc.html), or
[`base::sprintf()`](https://rdrr.io/r/base/sprintf.html):

``` r

tibble(
  item  = c("Widget", "Gadget"),
  price = c(1999.5, 12.4),
  added = as.Date(c("2024-01-15", "2024-11-03"))
) |>
  mutate(
    price = scales::dollar(price),     # currency, via scales
    added = format(added, "%B %d, %Y") # a custom date format, via base R
  ) |>
  tblr() |>
  show_latex()
```

    \begin{center}
        \begin{booktabs}{
            colspec = {lll}
            }
            \toprule
            item   & price      & added             \\ 
            \midrule
            Widget & \$1,999.50 & January 15, 2024  \\ 
            Gadget & \$12.40    & November 03, 2024 \\ 
            \bottomrule
        \end{booktabs}
    \end{center}

The formatted `price` is now a text column, so the literal `$` it
contains is escaped to `\$` for you — write the plain character you want
and let `tblr` handle the escaping.

Columns you leave untouched are converted with sensible defaults:
numbers get two decimal places (integer and whole-valued columns
without), and dates, logicals, and other types are rendered as R would
display them.

## Escaping

Characters that are special in LaTeX (`&`, `%`, `$`, `_`, `#`, `{`, `}`,
`~`, `^`, `\`) are escaped automatically in **text columns**, so
ordinary data is safe:

``` r

tibble(account = c("R&D", "50% off", "a_b")) |>
  tblr() |>
  show_latex()
```

    \begin{center}
        \begin{booktabs}{
            colspec = {l}
            }
            \toprule
            account \\ 
            \midrule
            R\&D \\ 
            50\% off \\ 
            a\_b \\ 
            \bottomrule
        \end{booktabs}
    \end{center}

Two things to keep in mind:

- **Convert column types before
  [`tblr()`](https://turbanisch.github.io/tabularray/reference/tblr.md).**
  Escaping is decided from each column’s type when the table is
  rendered. If you format a column upstream (as in the price example
  above), it is treated as text and escaped like any other text column.

- **Labels, spanners, alignment, notes, and options are raw LaTeX.**
  Values you pass to
  [`set_column_labels()`](https://turbanisch.github.io/tabularray/reference/set_column_labels.md),
  [`set_column_spanner()`](https://turbanisch.github.io/tabularray/reference/set_column_spanner.md),
  [`set_colspec()`](https://turbanisch.github.io/tabularray/reference/set_colspec.md),
  [`set_source_notes()`](https://turbanisch.github.io/tabularray/reference/set_source_notes.md),
  [`set_interface()`](https://turbanisch.github.io/tabularray/reference/set_interface.md),
  and
  [`set_options()`](https://turbanisch.github.io/tabularray/reference/set_options.md)
  are inserted verbatim, so you can use LaTeX commands there — and you
  must escape any special characters yourself (e.g. write `R\&D`, not
  `R&D`).

``` r

df |>
  tblr() |>
  set_column_labels(value = "\\textbf{Value}") |>
  show_latex()
```

    \begin{center}
        \begin{booktabs}{
            colspec = {llr}
            }
            \toprule
            continent & country     & \textbf{Value} \\ 
            \midrule
            Europe    & Germany     & 0.17           \\ 
            Asia      & China       & 0.23           \\ 
            Asia      & Afghanistan & 11.30          \\ 
            \bottomrule
        \end{booktabs}
    \end{center}

## Grouping, spanners, and themes

Group rows with
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
before
[`tblr()`](https://turbanisch.github.io/tabularray/reference/tblr.md),
add spanning headers with
[`set_column_spanner()`](https://turbanisch.github.io/tabularray/reference/set_column_spanner.md)
(call it once per spanner row, bottom-up), and tune the appearance with
[`set_theme()`](https://turbanisch.github.io/tabularray/reference/set_theme.md):

``` r

starwars |>
  filter(homeworld == "Tatooine", !name %in% c("Anakin Skywalker", "Darth Vader")) |>
  select(name, height, mass, sex, birth_year) |>
  mutate(sex = stringr::str_to_title(sex)) |>
  group_by(sex) |>
  tblr(type = "float", caption = "Tatooine characters") |>
  set_column_labels(name = "", birth_year = "Birth Year") |>
  set_column_spanner(c(height, mass) ~ "Physical") |>
  set_theme(row_group_style = "panel") |>
  show_latex()
```

    \begin{center}
        \begin{talltabs}[
            caption = {Tatooine characters}
            ]{
            colspec = {lrrr}
            }
            \toprule
                               & \SetCell[c=2]{c} Physical        &      &            \\ 
            \cmidrule{2-3}
                               & height                           & mass & Birth Year \\ 
            \midrule
                               & \SetCell[c=3]{c} \textit{Male}   &      &            \\ 
            \cmidrule{2-4}
            Luke Skywalker     & 172                              & 77   & 19         \\ 
            Owen Lars          & 178                              & 120  & 52         \\ 
            Biggs Darklighter  & 183                              & 84   & 24         \\ 
            Cliegg Lars        & 183                              & NA   & 82         \\ 
            \addlinespace
                               & \SetCell[c=3]{c} \textit{None}   &      &            \\ 
            \cmidrule{2-4}
            C-3PO              & 167                              & 75   & 112        \\ 
            R5-D4              & 97                               & 32   & NA         \\ 
            \addlinespace
                               & \SetCell[c=3]{c} \textit{Female} &      &            \\ 
            \cmidrule{2-4}
            Beru Whitesun Lars & 165                              & 75   & 47         \\ 
            Shmi Skywalker     & 163                              & NA   & 72         \\ 
            \bottomrule
        \end{talltabs}
    \end{center}

The `type` argument selects the LaTeX environment: `"simple"` (default,
inline), `"float"` (numbered, captioned, supports notes), or `"break"`
(page-breaking).

## Using the output

The generated markup needs the `tabularray` package and its `booktabs`
library. In a LaTeX preamble:

``` latex
\usepackage{tabularray}
\UseTblrLibrary{booktabs}
```

A reasonably recent `tabularray` (2023 or later) is recommended. In
Quarto or R Markdown, the output is embedded verbatim — see the
[README](https://github.com/turbanisch/tabularray) for the YAML setup.
You can also copy the LaTeX into any editor.
