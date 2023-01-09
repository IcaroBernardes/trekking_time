# Code inspired by mindlessgreen's response in Stack Overflow:
# https://stackoverflow.com/questions/64597525/r-magick-square-crop-and-circular-mask

# 1. Initial setup ##############
## Loads libraries
library(dplyr)
library(glue)
library(magick)
library(purrr)
library(stringr)

## Defines the final dimensions of the images
dim_num <- 200
dim_str <- as.character(dim_num)

## Creates a new image with white background and a black circle
mask <- magick::image_draw(image_blank(dim_num, dim_num))
symbols(dim_num/2, dim_num/2, circles=(dim_num/2)-3, bg="black", inches=FALSE, add=TRUE)
dev.off()

## Gets a list of all downloaded images
images_list <- list.files("www/images", full.names = TRUE)

# 2. Image crop and scaling ##############
## Use purrr::walk to get the side-effects of the function
## and not an object as return
purrr::iwalk(images_list, function(img, i) {
  
  ### Indicate which image is being processed
  print(glue::glue("Processing image #{i}..."))
  
  
  ### Gets the path to the image
  image_path <- img
  
  ### Reads the image and converts it to a PNG
  ### (so it can have a transparent background)
  image <- image_path %>% 
    magick::image_read() %>% 
    image_convert("png")
  
  ### Gets the biggest dimension of the image
  dim_max <- magick::image_info(image)
  dim_max <- min(dim_max$width, dim_max$height)
  
  ### Crops the image into equal dimensions (towards its center-top)
  ### and scales it down to 200px x 200px
  image <- image %>% 
    magick::image_crop(glue::glue("{dim_max}x{dim_max}"), gravity = "North") %>% 
    magick::image_scale("200")
  
  ### Creates an image composite using the image and the circular mask
  cropped <- magick::image_composite(image, mask, operator = "copyopacity")
  
  ### Sets the background as transparent
  cropped <- magick::image_background(cropped, "transparent")
  
  ### Saves the result in another directory
  image_path <- stringr::str_replace(image_path, "www/images", "www/cropped")
  magick::image_write(cropped, image_path)
  
})
