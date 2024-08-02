# Full script ---- See scripts folder for individual steps.

# Create list of necessary packages
libs <- c("tidyverse",
          "ambient",
          "rayshader")

# Query whether packages are installed
installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == FALSE)){
  install.packages(
    libs[!installed_libs]
  )
}

# Load packages in R-session
invisible(lapply(libs, 
                 library, 
                 character.only = TRUE)
)

# Set seed count
seed <- sample(1:100, 1)

# Build simple grid 
new_grid <- function(n = 1000) {
  ambient::long_grid(
    x = seq(0, 1, length.out = n),
    y = seq(0, 1, length.out = n)
  )
}

new_grid()

# Function to create spatial noise
generate_noise <- function(x, y, seed = NULL) {
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

# Create canvas. This applies the generate_simplex function to the new_grid function.
canvas <- new_grid() |> 
  dplyr::mutate(paint = generate_fancy_noise(x, y, seed = seed))

canvas


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

# A discretise function changes a continuous variable and breaks it into sections
discretise <- function(x, n) {
  round(ambient::normalise(x) * n) / n
}

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

