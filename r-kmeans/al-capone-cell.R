# Finding a library that can read a JPEG image as a matrix was difficult.
# EBImage can do it.
# http://bioconductor.org/packages/release/bioc/vignettes/EBImage/inst/doc/EBImage-introduction.html

# installing:
# BiocManager::install("EBImage")
# BiocManager::install("CRImage")

library(EBImage)
library(CRImage)
color_img <- readImage('../data/al_capone_cell.jpg')

# image histogram
hist(color_img)

# display image as a raster image
display(color_img, method="raster")

# dimensions of the image
dim(color_img)

blue_channel <- EBImage::channel(color_img, 'blue')
green_channel <- EBImage::channel(color_img, 'green')
red_channel <- EBImage::channel(color_img, 'red')

display(EBImage::channel(red_channel, 'asred'), method="raster")
display(EBImage::channel(green_channel, 'asgreen'), method="raster")
display(EBImage::channel(blue_channel, 'asblue'), method="raster")

# Get 2D vector of colors
color_img_vec <-as.array(color_img)
dim(color_img_vec) <- c(1800*1200, 3)

# Run kmeans 
clusters <- kmeans(color_img_vec, 10)

# Get results
res <- fitted(clusters)
dim(res) <- c(1800, 1200, 3)
res <- Image(res)
colorMode(res) <- Color
display(res)
