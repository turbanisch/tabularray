library(tidyverse)
library(gt)
source("tblr-maker.R")


# dummy data --------------------------------------------------------------

# mtcars
df <- mtcars[1:5, 1:3] |> as_tibble(rownames = "model")
colnames(df) <- c("Model", "MPG", "Cylinders", "Displacement (mm)")
df[3, 1] <- "Datsun Model 710"
df[4, 1] <- "Hornet US$"

# tracking table
df <- tibble(
  HS2 = c("30", "90"),
  Description = c(
    "Pharmaceutical products",
    "Optical, photographic, cinematographic, measuring, checking, medical
or surgical instruments and apparatus; parts and accessories"
  ),
  Value = c(806123123.23, 543015442)
)

# multiple column types
df <- tibble(
  model = c("Mazda", "Mercedes $4", "Audi", "Hyunday"),
  badass = c(TRUE, TRUE, FALSE, TRUE),
  cyl = c(4L, 6L, 6L, 4L),
  type = factor(c("SUV", "Sports & car", "SUV", "Sports & car"))
)


# function call -----------------------------------------------------------
tblr(
  df,
  caption = "A caption",
  source_notes = c(Notes = "Values in million USD.", Sources = "Own work and that of my colleagues.")
) |> 
 tblr_as_latex() |> 
  writeLines()

tblr(
  df,
  type = "draft",
  booktabs = FALSE,
  caption = "blah",
  interface = list(width = "\\linewidth")
) |> tblr_as_latex() |> writeLines()

tblr(df) |> attributes()
tblr(df, interface = list("colspec" = "cccc")) |> attributes()
tblr(df, options = list(caption = "blah")) |> tblr_as_latex() |> writeLines()
tblr(df, options = list(caption = "blah"), caption = "take me instead!") |> tblr_as_latex() |> writeLines()

intf <- tblr(df, interface = list(colspec = "lll", width = "\\linewidth")) |> 
  attr("interface")


# gt syntax ---------------------------------------------------------------

table1 |>
  gt() |>
  tab_header(title = "Top 15 Countries Receiving Chinese Aid Exports, 2017–2021") |>
  tab_source_note(md("*Notes:* We now use the current population size of each year (instead of holding it fixed at 2020).")) |>
  cols_label(
    rank = "Rank",
    country = "Country",
    aid_total = "Total aid (M$)",
    share = "Share (%)",
    aid_total_pc = "Per capita ($)",
    aid_med = "Medical aid (M$)",
    unofficial_aid = "Unofficial aid (M$)",
    mask_period = "Mask diplomacy period (M$)",
    vaccine_period = "Vaccine diplomacy period (M$)"
  ) |>
  fmt_number(columns = share, scale_by = 1e2) |> 
  fmt_number(columns = !c(rank, country, share, aid_total_pc),
             scale_by = 1e-6) |> 
  fmt_number(aid_total_pc, decimals = 2)

# gt syntax ---------------------------------------------------------------

