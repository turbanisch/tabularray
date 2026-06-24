# Output a **tblr** object as LaTeX

Generate LaTeX markup to typeset a `tblr` object. The output is a
character string of class `knit_asis` that is automatically included
verbatim in R Markdown documents.

## Usage

``` r
tblr_as_latex(x)
```

## Arguments

- x:

  A `tblr` table object.

## Value

A character string containing LaTeX markup of class `knit_asis`.

## Details

Make sure to load the necessary LaTeX packages in your document. In a
Quarto document's YAML metadata, please include


    format:
      pdf:
        include-in-header: tabularray-packages.sty

The `tabularray_packages.sty` file would then contain the dependencies
listed below:


    \usepackage{tabularray}
    \UseTblrLibrary{booktabs}

You do not need to modify any chunk options. `knitr` will automatically
embed the LaTeX markup verbatim.

## Examples

``` r
x <- tblr(mtcars)
tblr_as_latex(x)
#> [1] "\\begin{center}\n    \\begin{booktabs}{\n        colspec = {rrrrrrrrrrr}\n        }\n        \\toprule\n        mpg   & cyl & disp   & hp  & drat & wt   & qsec  & vs & am & gear & carb \\\\ \n        \\midrule\n        21.00 & 6   & 160.00 & 110 & 3.90 & 2.62 & 16.46 & 0  & 1  & 4    & 4    \\\\ \n        21.00 & 6   & 160.00 & 110 & 3.90 & 2.88 & 17.02 & 0  & 1  & 4    & 4    \\\\ \n        22.80 & 4   & 108.00 & 93  & 3.85 & 2.32 & 18.61 & 1  & 1  & 4    & 1    \\\\ \n        21.40 & 6   & 258.00 & 110 & 3.08 & 3.21 & 19.44 & 1  & 0  & 3    & 1    \\\\ \n        18.70 & 8   & 360.00 & 175 & 3.15 & 3.44 & 17.02 & 0  & 0  & 3    & 2    \\\\ \n        18.10 & 6   & 225.00 & 105 & 2.76 & 3.46 & 20.22 & 1  & 0  & 3    & 1    \\\\ \n        14.30 & 8   & 360.00 & 245 & 3.21 & 3.57 & 15.84 & 0  & 0  & 3    & 4    \\\\ \n        24.40 & 4   & 146.70 & 62  & 3.69 & 3.19 & 20.00 & 1  & 0  & 4    & 2    \\\\ \n        22.80 & 4   & 140.80 & 95  & 3.92 & 3.15 & 22.90 & 1  & 0  & 4    & 2    \\\\ \n        19.20 & 6   & 167.60 & 123 & 3.92 & 3.44 & 18.30 & 1  & 0  & 4    & 4    \\\\ \n        17.80 & 6   & 167.60 & 123 & 3.92 & 3.44 & 18.90 & 1  & 0  & 4    & 4    \\\\ \n        16.40 & 8   & 275.80 & 180 & 3.07 & 4.07 & 17.40 & 0  & 0  & 3    & 3    \\\\ \n        17.30 & 8   & 275.80 & 180 & 3.07 & 3.73 & 17.60 & 0  & 0  & 3    & 3    \\\\ \n        15.20 & 8   & 275.80 & 180 & 3.07 & 3.78 & 18.00 & 0  & 0  & 3    & 3    \\\\ \n        10.40 & 8   & 472.00 & 205 & 2.93 & 5.25 & 17.98 & 0  & 0  & 3    & 4    \\\\ \n        10.40 & 8   & 460.00 & 215 & 3.00 & 5.42 & 17.82 & 0  & 0  & 3    & 4    \\\\ \n        14.70 & 8   & 440.00 & 230 & 3.23 & 5.34 & 17.42 & 0  & 0  & 3    & 4    \\\\ \n        32.40 & 4   & 78.70  & 66  & 4.08 & 2.20 & 19.47 & 1  & 1  & 4    & 1    \\\\ \n        30.40 & 4   & 75.70  & 52  & 4.93 & 1.61 & 18.52 & 1  & 1  & 4    & 2    \\\\ \n        33.90 & 4   & 71.10  & 65  & 4.22 & 1.83 & 19.90 & 1  & 1  & 4    & 1    \\\\ \n        21.50 & 4   & 120.10 & 97  & 3.70 & 2.46 & 20.01 & 1  & 0  & 3    & 1    \\\\ \n        15.50 & 8   & 318.00 & 150 & 2.76 & 3.52 & 16.87 & 0  & 0  & 3    & 2    \\\\ \n        15.20 & 8   & 304.00 & 150 & 3.15 & 3.44 & 17.30 & 0  & 0  & 3    & 2    \\\\ \n        13.30 & 8   & 350.00 & 245 & 3.73 & 3.84 & 15.41 & 0  & 0  & 3    & 4    \\\\ \n        19.20 & 8   & 400.00 & 175 & 3.08 & 3.85 & 17.05 & 0  & 0  & 3    & 2    \\\\ \n        27.30 & 4   & 79.00  & 66  & 4.08 & 1.94 & 18.90 & 1  & 1  & 4    & 1    \\\\ \n        26.00 & 4   & 120.30 & 91  & 4.43 & 2.14 & 16.70 & 0  & 1  & 5    & 2    \\\\ \n        30.40 & 4   & 95.10  & 113 & 3.77 & 1.51 & 16.90 & 1  & 1  & 5    & 2    \\\\ \n        15.80 & 8   & 351.00 & 264 & 4.22 & 3.17 & 14.50 & 0  & 1  & 5    & 4    \\\\ \n        19.70 & 6   & 145.00 & 175 & 3.62 & 2.77 & 15.50 & 0  & 1  & 5    & 6    \\\\ \n        15.00 & 8   & 301.00 & 335 & 3.54 & 3.57 & 14.60 & 0  & 1  & 5    & 8    \\\\ \n        21.40 & 4   & 121.00 & 109 & 4.11 & 2.78 & 18.60 & 1  & 1  & 4    & 2    \\\\ \n        \\bottomrule\n    \\end{booktabs}\n\\end{center}"
#> attr(,"class")
#> [1] "knit_asis"
#> attr(,"knit_cacheable")
#> [1] NA
```
