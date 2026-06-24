# Regenerates the README example screenshots in man/figures/ from the *current*
# package output, so they never drift from the code again. Each example table is
# rendered to LaTeX, compiled with tabularray (standalone, tightly cropped), and
# exported to PNG.
#
# Requires a LaTeX toolchain (pdflatex) and poppler's pdftoppm. These are used
# only when regenerating the images, never at package build or use time.

library(dplyr)
devtools::load_all(".", quiet = TRUE)

render_png <- function(latex, outfile, dpi = 600) {
  # drop the outer \begin{center} ... \end{center} so `standalone` (varwidth)
  # crops tightly to the table instead of a full \linewidth-wide centred box
  lines <- strsplit(latex, "\n", fixed = TRUE)[[1]]
  lines <- lines[!grepl("^\\s*\\\\(begin|end)\\{center\\}\\s*$", lines)]
  body <- paste(lines, collapse = "\n")

  doc <- c(
    "\\documentclass[border=10pt,varwidth=16cm]{standalone}",
    "\\usepackage{tabularray}",
    "\\UseTblrLibrary{booktabs}",
    "\\begin{document}",
    body,
    "\\end{document}"
  )
  wd <- tempfile("tblr-fig")
  dir.create(wd)
  tex <- file.path(wd, "fig.tex")
  writeLines(doc, tex)

  # two passes: tabularray resolves column widths / SetCell on the second run
  for (i in 1:2) {
    status <- system2(
      "pdflatex",
      c("-interaction=nonstopmode", "-halt-on-error", "-output-directory", wd, tex),
      stdout = FALSE, stderr = FALSE
    )
  }
  pdf <- file.path(wd, "fig.pdf")
  if (!file.exists(pdf)) stop("pdflatex failed for ", outfile)

  # PDF -> PNG (white background; already cropped by standalone). pdftocairo
  # anti-aliases text more smoothly than pdftoppm.
  system2("pdftocairo", c("-png", "-r", dpi, "-singlefile", pdf,
                          tools::file_path_sans_ext(outfile)))
  invisible(outfile)
}

# the two README examples ----------------------------------------------------
df <- starwars |>
  filter(homeworld == "Tatooine", !name %in% c("Anakin Skywalker", "Darth Vader")) |>
  select(name, height, mass, sex, birth_year) |>
  arrange(desc(birth_year))
df[1, 1] <- "C$PO"

simple <- as.character(tblr_as_latex(tblr(df)))

markup <- as.character(tblr_as_latex(
  df |>
    mutate(sex = stringr::str_to_title(sex)) |>
    group_by(sex) |>
    tblr(type = "float", caption = "Starwars Creatures from Tatooine") |>
    set_source_notes(
      Note = "Entry C3PO altered to test characters that have a special meaning in LaTeX.",
      Source = "R package \\texttt{dplyr}"
    ) |>
    set_alignment(height:birth_year ~ "X[r]") |>
    set_column_labels(
      name = "", height = "Height", mass = "Mass", birth_year = "Birth Year"
    ) |>
    set_theme(row_group_style = "panel") |>
    set_interface(width = "0.7\\linewidth") |>
    set_column_spanner(c(height, mass) ~ "Group 1", birth_year ~ "Group 2") |>
    set_column_spanner(!name ~ "All my vars")
))

dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
render_png(simple, "man/figures/simple-table.png")
render_png(markup, "man/figures/marked-up-table.png")
cat("wrote man/figures/simple-table.png and man/figures/marked-up-table.png\n")
