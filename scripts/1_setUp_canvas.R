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

# Function to create spatial noise. The parameters in this function are the type of noise desired. 
# These are the contents of the ambient package. 
generate_noise <- function(x, y, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  ambient::fracture(
    noise = ambient::gen_simplex,
    fractal = ambient::billow,
    octaves = 10,
    freq_init = .02,
    frequency = ~ . * 2,
    gain_init = 1,
    gain = ~ . * .8,
    x = x,
    y = y
  )
}

# Create canvas. This applies the generate_simplex function to the new_grid function.
canvas <- new_grid() |> 
  dplyr::mutate(paint = generate_simplex(x, y, seed = seed))

canvas
