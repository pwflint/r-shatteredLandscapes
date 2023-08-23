# Bitmap matrix 
bitmap <- canvas |> as.array(value = paint)
bitmap[1:6, 1:6]

# Coerce canvas to bitmap matrix
canvas |> 
  as.array(value = paint) |>
  image(axes = FALSE, asp = 1, useRaster = TRUE)

# Create render function
render <- function(mat, shades = NULL, zscale = .005) {
  if(is.null(shades)) {
    n <- length(unique(mat))
    shades <- hcl.colors(n, "YlOrRd", rev = TRUE)
  }
  rayshader::height_shade(
    heightmap = mat,
    texture = shades
  ) |>
    rayshader::add_shadow(
      shadowmap = rayshader::ray_shade(
        heightmap = mat,
        sunaltitude = 50,
        sunangle = 80,
        multicore = TRUE,
        zscale = zscale
      ),
      max_darken = .2
    ) |>
    rayshader::plot_map()
}

# Switch rendering style on canvas
canvas |>
  as.array(value = paint) |>
  render()

# Create sea level datum
sea_level <- median(canvas$paint)

# Create Islands
canvas |> 
  dplyr::mutate(
    islands = dplyr::if_else(
      condition = paint < sea_level,
      true = sea_level, 
      false = paint
    )
  ) |>
  as.array(value = islands) |>
  render()

