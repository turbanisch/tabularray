# explore: store formatting function in list
dat <- list(
  c(1, 4, 6),
  c(3, 3, 3)
)

fns <- list(
  format,
  gt::vec_fmt_currency
)

map2(dat, fns, \(dat, f) f(dat))

set_formatter(mpg, vec_fmt_currency)