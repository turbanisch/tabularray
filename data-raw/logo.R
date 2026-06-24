# Generates man/figures/logo.png — a hex sticker for the package.
#
# Concept: "tabularray" = tabula (table) + array. A booktabs-style 2x2 table
# holding the letters of the main function `tblr` (T, B, L, R), flanked by
# square brackets that evoke an array. Drawn entirely in ggplot2 (no generative
# art, no extra dependencies) so it is fully reproducible and easy to tweak.

library(ggplot2)

# palette ------------------------------------------------------------------
bg      <- "#2E4756"  # slate background
border  <- "#88C0B8"  # sea-green hex border
ink     <- "#F5F0E6"  # cream: rules, letters, wordmark
bracket <- "#E8B04B"  # amber array brackets

# pointy-top hexagon (6 vertices; width:height ~ 0.866, the sticker ratio) --
ang <- (90 + 60 * (0:5)) * pi / 180
hex <- data.frame(x = cos(ang), y = sin(ang))

# 2x2 table geometry (kept well inside the hex; table sits in the upper half)
xL <- -0.32; xR <- 0.32; xM <- 0                 # column edges + divider
yB <- 0.04; yT <- 0.60; yMid <- (yB + yT) / 2    # row edges + mid rule
cx <- c(-0.16, 0.16)                             # column centres
cy <- c(yMid + (yT - yMid) / 2, yB + (yMid - yB) / 2)  # row centres (upper, lower)

cells <- data.frame(
  x = c(cx[1], cx[2], cx[1], cx[2]),
  y = c(cy[1], cy[1], cy[2], cy[2]),
  lab = c("T", "B", "L", "R")
)

# square brackets flanking the table ---------------------------------------
bx <- 0.46; serif <- 0.08
brackets <- rbind(
  data.frame(grp = "l", x = c(-bx + serif, -bx, -bx, -bx + serif),
             y = c(yT, yT, yB, yB)),
  data.frame(grp = "r", x = c(bx - serif, bx, bx, bx - serif),
             y = c(yT, yT, yB, yB))
)

p <- ggplot() +
  # hexagon
  geom_polygon(data = hex, aes(x, y), fill = bg, colour = border, linewidth = 3) +
  # booktabs-style rules: top / mid / bottom (no vertical lines in booktabs,
  # but a faint divider helps read the 2x2 grid)
  annotate("segment", x = xL, xend = xR, y = yT,   yend = yT,   colour = ink, linewidth = 1.6) +
  annotate("segment", x = xL, xend = xR, y = yMid, yend = yMid, colour = ink, linewidth = 0.9) +
  annotate("segment", x = xL, xend = xR, y = yB,   yend = yB,   colour = ink, linewidth = 1.6) +
  annotate("segment", x = xM, xend = xM, y = yB,   yend = yT,   colour = ink, linewidth = 0.3, alpha = 0.5) +
  # array brackets
  geom_path(data = brackets, aes(x, y, group = grp), colour = bracket, linewidth = 2.0, lineend = "round") +
  # T B L R
  geom_text(data = cells, aes(x, y, label = lab), colour = ink,
            family = "sans", fontface = "bold", size = 7) +
  # wordmark
  annotate("text", x = 0, y = -0.40, label = "tabularray", colour = ink,
           family = "sans", fontface = "bold", size = 5) +
  coord_fixed(xlim = c(-0.9, 0.9), ylim = c(-1, 1), expand = FALSE) +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA)
  )

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("man/figures/logo.png", p, width = 43.9, height = 50.8,
       units = "mm", dpi = 600, bg = "transparent")
