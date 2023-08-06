# bugs
- enclose numeric values with $ or \num{}
- implement options and interface as list of character *vectors* to allow entries like `vlines = {1,3,5}{dashed},`, i.e. `options = list(vlines = c("1,3,5", "dashed"))`

# general feature enhancements
- experiment with storing formatting functions and column types in df
- outsource anything that is not necessary from `tblr()` into other functions, e.g., `set_options()`, `set_interface()`, `set_colspec()`, `add_sourcenote()`.
- implement column spanners
- implement grouping
- replace TRUE and FALSE with checkmark
- check NA value and add option to change it
- add colspec shortcut
- implement footnotes

# features and compatibility with regression output
- either use formatted (text) output from modelsummary: `output = "data.frame"`
- or use actual values from modelsummary object: `x <- modelsummary::modelsummary(lm.D9, output = "modelsummary_list") |> unclass()`
- implement panels

# appearance & minor tweaks
- test including a citekey in notes
- test formatting with dates
- reduce font size in table notes
- align column heads with line breaks to bottom
- center column heads over numeric values (or all narrow columns?). The stub head should always be left-aligned: 3.71
- make knitr include `\usepackage{tabularray}` etc. automatically in preamble