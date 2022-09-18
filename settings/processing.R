library(tidyverse)
library(magick)

# Image processing
image_read(path = "docs/images/hero_image_cropped_short.png") %>%
  image_write(format = "svg", 
              path = "docs/images/hero_image_cropped_short.svg", 
              quality = 100)
# Image processing
image_read(path = "docs/images/hero_image_cropped_tall.png") %>%
  image_write(format = "svg", 
              path = "docs/images/hero_image_cropped_tall.svg", 
              quality = 100)
