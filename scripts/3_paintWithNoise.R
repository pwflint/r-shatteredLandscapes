# Fancy noise from createMap.r script. 
# This is probably the most complex script.
generate_fancy_noise <- function(x, y, seed = NULL) {
   # Set the seed value from the `seed` object in setUp_canvas.r 
   if(!is.null(seed)) {
    set.seed(seed)
   }
  # Create z heights that rayshader will use to render.
  z <- ambient::fracture(
    noise = ambient::gen_worley,
    fractal = ambient::billow,
    octaves = 8,
    freq_init = .1,
    frequency = ~ . * 2,
    gain_init = 3,
    gain = ~ . * .5,
    value = "distance2",
    x = x,
    y = y
  )
  ambient::fracture(
    noise = ambient::gen_simplex,
    fractal = ambient::billow,
    octaves = 10,
    freq_init = .02,
    frequency = ~ . * 2,
    gain_init = 1,
    gain = ~ . * .8,
    x = x + z,
    y = y + z
  )
}

new_grid() |>
  dplyr::mutate(
    height = generate_fancy_noise(x, y, seed = seed),
    islands = dplyr::if_else(
      condition = height < median(height),
      true = median(height),
      false = height
    )
  ) |>
  as.array(value = islands) |>
  render(zscale = .01)




