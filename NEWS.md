# tabularray (development version)

## Bug fixes

* Numbers are now formatted with a fixed number of decimal places (2 by
  default) instead of significant figures. Values such as `3.04` are no longer
  truncated to `"3"`, and formatting no longer depends on the other values in a
  column. Integer and whole-valued columns render without decimals; dates and
  other types pass through faithfully. Per-column currency/percent/significant-
  digit formatting remains the user's job, done before `tblr()` with packages
  such as scales or gt.

* Column spanners no longer error with recent versions of dplyr (whose stricter
  `if_else()` rejected the array passed internally).

## Improvements

* Whether a column's contents are escaped is now determined from the column's
  type at render time rather than recorded when `tblr()` is called. Columns
  formatted before *or* after `tblr()` are now both escaped correctly.

* Functions emit informative errors (e.g. when a caption is missing, more than
  one grouping variable is supplied, or `colspec` is set via `set_interface()`),
  and warn when `set_alignment()` / `set_column_labels()` are given a column
  name that does not exist.

## Dependencies

* Removed the dependency on **gt**; LaTeX escaping is now built in.

* **knitr** is no longer required to generate LaTeX output. It remains a
  suggested dependency for seamless R Markdown / Quarto integration.

* Declared a dependency on R (>= 4.1.0), which the package already required
  (native pipe and `\(x)` lambda syntax).
