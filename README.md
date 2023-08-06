## Why evaluate twice?

One idea is to prepare the dataframe including subsetting of columns, transformation of columns (e.g., rounding) before passing it on to the table function. That way we would only have to handle a single "flat" dataframe, rather than one with additional attributes. However, there are things that cannot be reflected in a dataframe, such as captions, footnotes, spanner column labels, grouping columns etc. So it does make sense to store both the original dataset and additional markup in a list or in a dataframe augmented by attributes.

In the current implementation, the dataframe we would like to format as a table is evaluated twice: `tblr()` adds attributes and sets the dataframe's class to `tblr`. We can then modify the dataframe as usual, e.g. add formatting with `gt::vec_fmt_*()` functions. Finally, we generate LaTeX code using `tblr_to_latex()`. You don't normally need to call this function yourself as it is invoked automatically as a `print()` method and when knitting a document.

Wouldn't it be sufficient to just evaluate the dataframe once, at the very end? No, because there are certain properties we need to derive from the original dataframe, before any modifications. One example is the column alignment. For example, numeric content should be right-aligned in the table. However, functions like `gt::vec_fmt_number()` convert numeric columns to columns of type character and there is no (simple) way to retrieve the original column specification.

The properties we need to derive from the original dataframe mostly pertain to column types. First, as described above, we use the basic distinction "text-like" (character vectors and factors) vs. "numeric" to tentatively set the alignment (`colspec` in LaTeX).
Second, we use column types from the original dataframe to determine in which columns we need to escape characters that have a special meaning in LaTeX. Assuming that any manual formatting can only turn columns that weren't text-like initially into columns that are, there are three cases when the LaTeX code is generated: 

 1. columns that remained non-text-like, e.g., numeric ones,
 2. columns that were initially text-like,
 3. columns that have been turned into text-like ones.

Each of the three cases receives a different treatment. We rely on `format(..., trim = TRUE)` to format columns that have remained non-text-like. We then escape special LaTeX characters in columns that were originally text-like. Any additional columns that are text-like *at the end* are assumed to have been produced by manually applying formatting functions that take care of escaping special characters. For example, when you call `gt::vec_fmt_number()` in a knitr environment, it automatically escapes special LaTeX characters. You can force this behavior using the `output = "latex"` argument.