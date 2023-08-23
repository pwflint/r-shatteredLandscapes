# Creating artistic trickery

# A discretise function changes a continuous variable and breaks it into sections
discretise <- function(x, n) {
  round(ambient::normalise(x) * n) / n
}

# Pipe `discretize` object into original `canvas` object
grid <- new_grid() 
coords <- generate_curl(grid$x, grid$y, seed = seed)

canvas <- grid |> 
  dplyr::mutate(
    curl_x = coords$x |> discretise(50), 
    curl_y = coords$y |> discretise(50),
    height = generate_fancy_noise(curl_x, curl_y, seed = seed) |> 
  discretise(50),
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
# This returns a pixelly amorphous blob

canvas |> 
  as.array(value = islands) |>
  render(zscale = .05)
# This returns the pixelly amorphous blob with more defined contours

# Smoothing. This replaces the height argument with manipulations of the spatial_noise and generate_simplex functions
grid <- new_grid() 
coords <- generate_curl(grid$x, grid$y, seed = seed)

canvas <- grid |> 
  dplyr::mutate(
    curl_x = coords$x |> discretise(50), 
    curl_y = coords$y |> discretise(50),
    noise_curl = generate_fancy_noise(curl_x, curl_y, seed = seed),
    noise_base = generate_simplex(x, y, seed = seed),
    height = (noise_curl + noise_base) |> discretise(50),
    islands = dplyr::if_else(
      condition = height < median(height),
      true = median(height),
      false = height
    )
  ) 

canvas |> 
  as.array(value = islands) |>
  render(zscale = .01) 

# Color and Chaos

# Create color variables
shades <- hcl.colors(50, "TealGrn")
shades[1] <- "#ffffff"

canvas |> 
  as.array(value = islands) |>
  render(shades = shades) 

# Shuffle colors off contour (z) heights
generate_shades <- function(palette = "TealGrn", n = 50, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  shades <- hcl.colors(n, palette)
  shades <- sample(shades)
  shades[1] <- "#ffffff"
  shades  
}

canvas |> 
  as.array(value = islands) |>
  render(shades = generate_shades(seed = seed)) 
