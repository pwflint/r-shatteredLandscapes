# Generate curls
generate_curl <- function(x, y, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  ambient::curl_noise(
    generator = ambient::fracture,
    noise = ambient::gen_simplex,
    fractal = ambient::fbm,
    octaves = 3,
    frequency = ~ . * 2,
    freq_init = .3,
    gain_init = 1,
    gain = ~ . * .5,
    x = x,
    y = y
  )
}

grid <- new_grid()
coords <- generate_curl(grid$x, grid$y, seed = seed)
head(coords)

canvas <- grid |>
  dplyr::mutate(
    curl_x = coords$x,
    curl_y = coords$y
  )

canvas

canvas <- canvas |>
  dplyr::mutate(
    height = generate_fancy_noise(curl_x, curl_y, seed = seed),
    islands = dplyr::if_else(
      condition = height < median(height),
      true = median(height),
      false = height
    )
  )

canvas

canvas |> 
  as.array(value = islands) |>
  image(axes = FALSE, asp = 1, useRaster = TRUE)

