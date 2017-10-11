
# Methods to build own color palette are presented in this script.

# Create palette with user defined colors -----------------------------------------------------

# devtools::install_github("MarcoDVisser/choosecolor")

library("choosecolor")

# pick a color
mycol <- color.choose()

# create a palette by selecting n = 5 colors
Mypalette <- palette.picker(n = 5)

# the palette can then have as many colors as needed
Mypalette(10)

# Get more detail in choice with the color wheel
color.wheel()


# Color palette from an image -----------------------------------------------------------------

# from https://www.r-bloggers.com/how-to-build-a-color-palette-from-any-image-with-r-and-k-means-algo/
# add possibility to use png image
# does not give exact color on logo, should only be used on image to have a large set of colors

palette_maker <- function(image_path, number_of_colors = 10){

  if(!file.exists(image_path)) stop("The provided image cannot be found, please check that the file exists.")

  png <- stringi::stri_detect_regex(str = image_path, pattern = '.png$')
  if(png){
    painting <- png::readPNG(image_path)
  } else{
    painting <- jpeg::readJPEG(image_path)
  }
  dimension <- dim(painting)
  painting_rgb <- data.frame(x = rep(1:dimension[2], each = dimension[1]),
                             y = rep(dimension[1]:1, dimension[2]),
                             R = as.vector(painting[, , 1]),
                             G = as.vector(painting[, , 2]),
                             B = as.vector(painting[, , 3]))
  k_means <- kmeans(painting_rgb[, c("R", "G", "B")], centers = number_of_colors,
                    iter.max = 1000)
  colours_k <- rgb(k_means$centers[k_means$cluster, ])
  colours_vector <- unique(colours_k)
  scales::show_col(colours_vector)

  return(colours_vector)
}

palette_maker(image_path = 'input/logo bouygue.png', number_of_colors = 4)
