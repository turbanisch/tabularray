# Set table interface

This function gives you low-level access to the mandatory argument of a
`tblr` environment, also called "inner specifications". In LaTeX, this
section is primarily used to define the column specification. The
`tabularray` LaTeX package provides a so-called "new interface" that
extends the functionality of the mandatory argument and allows targeting
individual rows, columns, and cells.

## Usage

``` r
set_interface(x, ...)
```

## Arguments

- x:

  A `tblr` table object.

- ...:

  Named arguments to be spliced into key-value pairs within the
  mandatory argument of a `tblr` environment.

## Value

A `tblr` table object.

## Details

The mandatory argument is a list of (mostly) key-value pairs. Use named
arguments to specify them with this function. Both the names and values
you provide are assumed to be raw LaTeX strings; characters that have a
special meaning in LaTeX will not be escaped. To set a key without a
value (e.g., "hlines"), pass `character(0)` as in the example below.
Note that the value can be a vector with more than one element. Each
element is automatically wrapped in curly braces.

Shortcut functions targeting specific parts of the interface such as
[`set_alignment()`](https://turbanisch.github.io/tabularray/reference/set_alignment.md)
take precedence over the specifications you make here.

## Examples

``` r
# Produce the following specification in LaTeX:
# {
#   hlines,
#   vlines = {1, 3, 5}{dashed},
#   rows = {7mm}
# }

if (FALSE) { # \dontrun{
x |>
  set_interface(
    hlines = character(0),
    vlines = c("1,3,5", "dashed"),
    rows = "7mm"
  )
} # }
```
