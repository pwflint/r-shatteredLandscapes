# Three step to loading necessary packages for a project
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
