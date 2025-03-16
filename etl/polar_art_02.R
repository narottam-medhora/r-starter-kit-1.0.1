library(ggplot2)
library(tibble)

polar_art_02 <- function(n, seed, pallette) {
  # Set the seed
  set.seed(seed)

  data <- tibble(
    x0 = runif(n),
    y0 = runif(n),
    x1 = x0 + runif(n, min = -.5, max = .5),
    y1 = y0 + runif(n, min = -.5, max = .5),
    shade = runif(n),
    size = runif(n),
    alpha = runif(n)
  )

  data |> ggplot(
    aes(
      x = x0,
      y = y0,
      xend = x1,
      yend = y1,
      colour = shade,
      size = size,
      alpha = alpha
    )
  ) +
    geom_segment(show.legend = F) +
    coord_polar() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_colour_gradientn(colours = pallette) +
    scale_size(range = c(15, 20)) +
    theme_void()
}

polar_art_02(n = 500, seed = 1, pallette = c("goldenrod", "cornflowerblue", "tomato"))
polar_art_02(n = 1000, seed = 2, pallette = c("#432E54","#4B4376","#AE445A","#E8BCB9"))
polar_art_02(n = 10, seed = 2, pallette = c("#432E54","#4B4376","#AE445A","#E8BCB9"))
polar_art_02(n = 5, seed = 2, pallette = c("#432E54","#4B4376","#AE445A","#E8BCB9"))
polar_art_02(n = 50, seed = 2, pallette = c("#432E54","#4B4376","#AE445A","#E8BCB9"))