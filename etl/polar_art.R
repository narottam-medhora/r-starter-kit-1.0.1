library(ggplot2)
library(tibble)

mpg |>
  ggplot(aes(class, hwy, color = drv, size = cyl)) +
  geom_point(show.legend = FALSE, size = 4) +
  geom_point(show.legend = F, size = 1, colour = "#222222") +
  coord_polar() +
  theme_void() +
  scale_color_brewer()

# Generate a bunch of random numbers
set.seed(1)
n <- 50
data <- tibble(
  x0 = runif(n),
  y0 = runif(n),
  x1 = x0 + runif(n, min = -.2, max = .2),
  y1 = y0 + runif(n, min = -.2, max = .2),
  shade = runif(n),
  size = runif(n)
)

data |>
  ggplot(
    aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      color = shade,
      size = size
    )
  ) +
  geom_segment(show.legend = F) +
  coord_polar() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_viridis_c() +
  scale_size(range = c(0, 10)) +
  theme_void()

# Creating a polar art function
polar_art <- function(seed, n, palette) {
  # Set the seed generator
  set.seed(seed)

  # Data frame containing random values for aesthetics
  # we might want to use in the art
  data <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.2, max = .2),
    y1 = y0 + runif(n, min = -.2, max = .2),
    shade = runif(n),
    size = runif(n)
  )

  # plot segments in various colours, using polar coordinates
  #  and a gradient palette
  data |>
    ggplot(aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      size = size,
    )) +
    geom_segment(show.legend = F) +
    coord_polar() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_colour_gradientn(colours = palette) +
    scale_size(range = c(0, 10)) +
    theme_void()
}

# Varying n and palette changes the number of segments plotted and the colour
# scheme used
polar_art(seed = 1, n = 500, palette = c("antiquewhite", "orange", "bisque"))
polar_art(seed = 1, n = 500, palette = c("red", "black", "white"))
polar_art(seed = 2, n = 50, palette = c("red", "black", "white"))

# Variations
polar_art(seed = 1, n = 30, palette = c("#222831", "#393E46", "#00ADB5", "#EEEEEE"))
polar_art(seed = 1, n = 3000, palette = c("#222831", "#393E46", "#00ADB5", "#EEEEEE"))
polar_art(seed = 3, n = 100, palette = c("#254251", "#e0ab26"))
