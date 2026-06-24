# Set table options

This function gives you low-level access to the optional argument of a
`tblr` environment, also called "outer specifications". For example, for
types "float" and "break" the optional argument can contain the caption,
a label, and notes.

## Usage

``` r
set_options(x, ...)
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

The optional argument is a list of key-value pairs. Use named arguments
to specify them with this function. Both the names and values you
provide are assumed to be raw LaTeX strings; characters that have a
special meaning in LaTeX will not be escaped. Each value is
automatically wrapped in curly braces.

Shortcut functions targeting specific options such as the caption you
specify in the call to
[`tblr()`](https://turbanisch.github.io/tabularray/reference/tblr.md)
take precedence over the specifications you make here.
