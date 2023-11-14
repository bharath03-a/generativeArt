#> Title: Recreating generative art
#> Author: Bharath Velamala
#> Date: 09-11-2023
#> https://github.com/cj-holmes/flow-field-art

if (!require("pacman"))
  install.packages("pacman")


pacman::p_load(tidyverse,
               here,
               janitor,
               colorspace,
               showtext,
               magick,
               stars,
               sf)

shivling <- image_read(here("images", "Mt_shivling.jpeg")) |>
  image_resize("1000x")
shivling

# Define some variables for this step
size <- 1000
n_shades <- 30

shivling_processed <-
  shivling |>
  image_resize(paste0(size, "x", size, "^")) |>
  image_crop(geometry = paste0(size, "x", size), gravity = "center") |>
  image_convert(type = "grayscale") |>
  image_quantize(max = n_shades, dither = FALSE) |>
  image_flip()

shivling_processed

shivling_sf <-
  shivling_processed |>
  image_raster() |>
  mutate(col2rgb(col, alpha = FALSE) |> t() |> as_tibble(),
         col = scales::rescale(green, to = c(1, 0))) |>
  select(-green,-blue,-red) |>
  stars::st_as_stars() |>
  st_as_sf(as_points = FALSE, merge = TRUE) |>
  st_make_valid() |>
  st_set_agr("constant") |>
  st_normalize()

modified_plot <- ggplot() +
  geom_sf(data = shivling_sf, col = NA, aes(fill = col)) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  theme_void() +
  theme(legend.position = "none")

# saving the plot as an image file
ggsave(modified_plot,
       filename = "images/updated_shivling.png",
       dpi      = 500)

#' Create spiral coordinates
#'
#' @param xo Spiral origin x coordinate
#' @param yo Spiral origin y coordinate
#' @param n_points Number of points on whole spiral (equally spaced in angle)
#' @param n_turns Number of turns in spiral
#' @param r0 Spiral inner radius
#' @param r1 Spiral outer radius
#' @param offset_angle Offset angle for start of spiral (in degrees)
spiral_coords <-
  function(xo,
           yo,
           n_points,
           n_turns,
           r0,
           r1,
           offset_angle) {
    b <- (r1 - r0) / (2 * pi * n_turns)
    l <- seq(0, 2 * pi * n_turns, l = n_points)

    tibble(
      x = (r0 + (b * l)) * cos(l + offset_angle * (pi / 180)) + xo,
      y = (r0 + (b * l)) * sin(l + offset_angle * (pi / 180)) + yo
    )
  }

n_turns <- 50
spiral_r1 <- 0.5

spiral <-
  spiral_coords(
    xo = 0.5,
    yo = 0.5,
    n_points = 5000,
    n_turns = n_turns,
    r0 = 0,
    r1 = spiral_r1,
    offset_angle = 0
  ) |>
  as.matrix() |>
  sf::st_linestring()

spiral_plot <- ggplot() +
  geom_sf(data = shivling_sf, aes(fill = col), col = NA) +
  geom_sf(data = spiral, col = "black") +
  scale_fill_viridis_c("", alpha = 0.75, direction = -1) +
  theme(legend.position = "")

# saving the plot as an image file
ggsave(spiral_plot,
       filename = "images/spiral_shivling.png",
       dpi      = 500)

thin <- 0.00025
thick <- ((spiral_r1/n_turns)/2)*0.95

intersections <-
  st_intersection(shivling_sf, spiral) |>
  mutate(n = scales::rescale(col, to=c(thin, thick))) |>
  mutate(geometry = st_buffer(geometry, n, endCapStyle = "ROUND"))


intersect_plot <- ggplot() + geom_sf(data = intersections, fill = "black", col = NA) +
  theme_void() +
  theme(legend.position = "")

# saving the plot as an image file
ggsave(intersect_plot,
       filename = "images/intersect_shivling.png",
       dpi      = 500)

