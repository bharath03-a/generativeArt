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
  image_resize(paste0(size,"x",size,"^")) |>
  image_crop(geometry = paste0(size,"x",size), gravity = "center") |>
  image_convert(type = "grayscale") |>
  image_quantize(max = n_shades, dither=FALSE) |>
  image_flip()

shivling_processed

shivling_sf <-
  shivling_processed |>
  image_raster() |>
  mutate(
    col2rgb(col, alpha = FALSE) |> t() |> as_tibble(),
    col = scales::rescale(green, to = c(1,0))) |>
  select(-green, -blue, -red) |>
  stars::st_as_stars() |>
  st_as_sf(as_points = FALSE, merge = TRUE) |>
  st_make_valid() |>
  st_set_agr("constant") |>
  st_normalize()

modified_plot <- ggplot() +
  geom_sf(data = shivling_sf, col = NA, aes(fill = col)) +
  scale_fill_viridis_c(option="inferno", direction = -1) +
  theme_void() +
  theme(
    legend.position = "none"
  )

# saving the plot as an image file
ggsave(modified_plot,
       filename = "images/updated_shivling.png",
       dpi      = 500)