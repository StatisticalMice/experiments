# Finding a library that can read a JPEG image as a matrix was difficult.
# EBImage can do it.
# http://bioconductor.org/packages/release/bioc/vignettes/EBImage/inst/doc/EBImage-introduction.html

# installing:
# BiocManager::install("EBImage")
# BiocManager::install("CRImage")

library(EBImage)
color_img <- readImage('../data/al_capone_cell.jpg')

# image histogram
hist(color_img)

# display image as a raster image
display(color_img, method="raster")

# dimensions of the image
dim(color_img)

blue_channel <- channel(color_img, 'blue')
green_channel <- channel(color_img, 'green')
red_channel <- channel(color_img, 'red')

display(channel(red_channel, 'asred'), method="raster")
display(channel(green_channel, 'asgreen'), method="raster")
display(channel(blue_channel, 'asblue'), method="raster")




