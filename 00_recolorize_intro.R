# A general intro to how I'm handling color segmentation right now,
# with an example on a single image

# bookkeeping ####

# for plotting pixels in various color spaces:
install.packages("colordistance")

# to install packages from github:
install.packages("devtools")

# now install recolorize (not on CRAN yet):
devtools::install_github("hiweller/recolorize")

# and load the package
library(recolorize)

# image path (comes with the package)
corbetti <- system.file("extdata/corbetti.png", package = "recolorize")

# short version: magic!
corbetti_final <- recolorize2(corbetti, cutoff = 45)
names(corbetti_final)

# plotting for class 'recolorize':
plot(corbetti_final)

# the pixel assignment matrix is the most useful thing:
image(corbetti_final$pixel_assignments,
      asp = 0.48, col = grey.colors(6, 0, 1))

# you can also plot the palette in proportion to the size of each center:
plotColorPalette(corbetti_final$centers, 
                 corbetti_final$sizes)

# we can reconstitute the pixel assignments into an RGB image:
recolored_img <- recoloredImage(corbetti_final, type = "raster")
plot(recolored_img)

# or examine the residuals:
imdists <- imDist(recolorize:::raster_to_array(corbetti_final$original_img),
                  recolorize:::raster_to_array(recolored_img),
                  color_space = "sRGB")
hist(imdists)





# brace for long version with under-the-hood explanations:

# read/plot an image ####
# we can read it in as an array:
img <- png::readPNG(corbetti)
dim(img)
range(img) # 0-1 range (rather than 0-255)

# we can plot the RGB image using as.raster:
plot(as.raster(img))

# we can also plot each channel separately

# set nice plotting parameters
layout(matrix(1:5, 1)) # grid layout
par(mar = rep(0, 4)) # no margins

# plot original RGB first:
plot(as.raster(img))

# order is red, green, blue, and alpha channel (transparency):
apply(img, 3, function(i) plot(as.raster(i)))
# you can see we masked out the pin in the alpha channel only

# so recolorize treats this as a multi-dimensional segementation problem
# pixels = points, color channels (like R, G, B) = measurements/columns


# brief color space orientation ####

# a color space is a way of representing colors in several (usually 3) 
# dimensions where the axes represent different features of the colors,
# i.e. hue, brightness, saturation, redness, etc

# RGB is a weird color space because it's a cube; distances in
# RGB only very approximately correspond to perceived differences
# ex: black and red are as far apart as red and yellow are

# make 10K random RGB colors:
rgb_random <- matrix(runif(30000), ncol = 3) # 10k rgb colors
hex_colors <- rgb(rgb_random) # hex codes for plotting
scatterplot3d::scatterplot3d(rgb_random, pch = 19,
                             color = hex_colors,
                             xlab = "red", ylab = "green", zlab = "blue")
scatterplot3d::scatterplot3d(rgb_random, pch = 19,
                             angle = -45,
                             color = hex_colors,
                             xlab = "red", ylab = "green", zlab = "blue")

# a popular alternative is CIE Lab space, which is approximately perceptually
# uniform but is asymmetrical
# L = luminance channel (dark to bright)
# a = red-green channel
# b = blue-yellow channel
# the idea is that things can be greenish-yellow, but they can't really be
# reddish-green or bluish-yellow, etc
lab_random <- recolorize:::col2col(rgb_random,
                                   to = "Lab")

# blob time:
scatterplot3d::scatterplot3d(lab_random, pch = 19,
                             color = hex_colors,
                             xlab = "L", ylab = "a", zlab = "b")
scatterplot3d::scatterplot3d(lab_random, pch = 19,
                             angle = -45,
                             color = hex_colors,
                             xlab = "L", ylab = "a", zlab = "b")


# so we can also plot the image in RGB color space:
layout(matrix(1:2, nrow = 1))
par(mar = rep(1, 4))
colordistance::plotPixels(corbetti, color.space = "rgb", 
                          main = "RGB color space")
colordistance::plotPixels(corbetti, color.space = "rgb", 
                          main = "RGB color space", 
                          angle = -45)

# and in CIE Lab space:
colordistance::plotPixels(corbetti, color.space = "lab", 
                          ref.white = "D65",
                          main = "CIE Lab color space")
colordistance::plotPixels(corbetti, color.space = "lab", 
                          ref.white = "D65",
                          main = "CIE Lab color space", 
                          angle = -45)

# k-means clustering ####

# k-means clustering tries to (iteratively) find k centers
# to minimize within-cluster variances, which sounds great in theory,
# and kind of works in this case:
corbetti_k <- recolorize(corbetti, method = "k", n = 5)
plotColorClusters(corbetti_k$centers, 
                  corbetti_k$sizes, scaling = 20)

# denser regions will attract more clusters to reduce variance:
corbetti_k <- recolorize(corbetti, method = "k", n = 20)
plotColorClusters(corbetti_k$centers, 
                  corbetti_k$sizes, scaling = 20)

# cluster order changes every time:
corbetti_k <- recolorize(corbetti, method = "k", n = 5)

# and the number of clusters really affects the results:
corbetti_k <- recolorize(corbetti, method = "k", n = 4)
corbetti_k <- recolorize(corbetti, method = "k", n = 6)

# CIE Lab does somewhat better but is still iffy:
corbetti_k <- recolorize(corbetti, method = "k", n = 5,
                         color_space = "Lab")

# and this is an easy image! it's literally RGB extremes


# recolorize step 1: overcluster ####

# initial step (transparent background is ignored automatically)
corbetti_1 <- recolorize(corbetti, 
                         method = "hist",
                         bins = 3,
                         color_space = "sRGB") # too many colors (on purpose)

# quick comparison of what this is doing vs. kmeans:
# this is just so we can get an interactive plot; ignore warnings
corbetti_kmean <- colordistance::getKMeanColors(corbetti, n = 27,
                                               plotting = FALSE)
colordistance::plotClusters(colordistance::extractClusters(corbetti_kmean))

# vs histograms
corbetti_hist <- colordistance::getImageHist(corbetti, bins = 3,
                                             plotting = FALSE)
colordistance::plotClusters(corbetti_hist)




# recolorize step 2: recluster ####
# make a distance matrix of the color centers in some color space
# and cluster them:
hclust_color(corbetti_1$centers,
             dist_method = "euclidean",
             return_list = FALSE, cutoff = seq(40, 80, by = 10))

# then apply a cutoff below which centers will be grouped - so we're
# grouping clusters by similarity
corbetti_2 <- recluster(corbetti_1, 
                        color_space = "Lab",
                        similarity_cutoff = 60,
                        refit_method = "impose")

# we can do all this in one line:
corbetti_final <- recolorize2(corbetti, bins = 2, cutoff = 45)


# recolorize step 3: manual edits (pandora's box?) ####

# refit the original image, without cluster 4:
corbetti_3 <- imposeColors(corbetti, 
                           corbetti_2$centers[-4, ])

# you can also merge 4 & 5 (I kind of prefer this):
corbetti_merge <- mergeLayers(corbetti_2, merge_list = list(4:5))

# we can swap out the color centers:
new_centers <- c("goldenrod1", "darkorchid4", 
                 "palegreen", "hotpink", "dodgerblue")
new_centers <- t(col2rgb(new_centers) / 255) # RGB triplets in row order
swapped_centers <- constructImage(corbetti_3$pixel_assignments,
                                  centers = new_centers)
plot(as.raster(swapped_centers))

# we can edit layers:
corbetti_layerEdit <- editLayer(corbetti_3,
                                layer_idx = 1,
                                operation = "clean",
                                px_size = 4)
corbetti_mondrian <- editLayers(corbetti_3, 
                                layer_idx = "all", 
                                operations = "clean",
                                px_sizes = c(15, 15, 15, 8, 15))

# to split by layer:
layout(matrix(1:10, nrow = 2, byrow = TRUE))
layers <- splitByColor(corbetti_3, plot_method = "overlay")
lapply(layers, plotImageArray) # <- layers themselves are just masks

# to export to pavo:
corbetti_classify <- classify_recolorize(corbetti_3, imgname = "corbetti")


