# problem children

# textured fish (easily handled):
fish <- "Pygoplites_EKaran.png"
fish_fit <- recolorize2(fish, resize = 0.5, cutoff = 45)


# shiny beetle (some fussing):
ful <- system.file("extdata/fulgidissima.png", package = "recolorize")
ful1 <- recolorize2(ful, bins = 3, cutoff = 65)
ful2 <- absorbLayer(ful1, layer_idx = 3,
            size_condition = function(s) s <= 250,
            y_range = c(0, 0.8))

# shiny snake (more fussing):
snake <- readImage("Micrurus_ADavisRabosky.png", resize = 0.4)
blur_snake <- blurImage(snake, "blur_anisotropic",
                        amplitude = 10, sharpness = 0.01)
snake_1 <- recolorize2(blur_snake, bins = 3, cutoff = 30)
snake_2 <- mergeLayers(snake_1, list(c(2, 3)), 
                       color_to = snake_1$centers[3, ])
rm(list = ls(pattern = "*snake*"))

# batch processing:
# get 5 beetle images:
images <- dir(system.file("extdata", package = "recolorize"), "png",
              full.names = TRUE)

# very simple binning & reclustering:
for (i in 1:length(images)) {
  init_fit <- recolorize2(images[i], bins = 2, cutoff = 30, plotting = F)
  final_fit <- thresholdRecolor(init_fit)
  if (i == 1) {
    colormap_list <- list(final_fit)
  } else {
    colormap_list[[i]] <- final_fit
  }
}

# plot color maps
lapply(colormap_list, plot)

# get adjacency metrics using recolorize_adjacency
adjacency_metrics <- lapply(colormap_list, recolorize_adjacency)
# note: these values come with a BOULDER of salt because 
# they are using human perceptual color distances (in CIE Lab space)

# ideally, you generate a pavo 'coldist' object using spectral reflectance
# measurements taken per patch, corresponding to the color map

# but they work ok for a demo, because you, a human, are perceiving them

# clean up the adjacency analysis output:
for (i in 1:length(adjacency_metrics)) {
  vals <- adjacency_metrics[[i]][grep("m_dL|m_dS", 
                                      colnames(adjacency_metrics[[i]]))]
  if (i == 1) {
    boundary_stuff <- as.data.frame(vals)
  } else {
    boundary_stuff <- rbind(boundary_stuff, 
                            vals)
  }
}

# plot boundary strength:
plot(boundary_stuff$m_dL,
     boundary_stuff$m_dS,
     xlab = "Boundary strength (luminance)",
     ylab = "Boundary strength (color)",
     xlim = c(8, 42),
     ylim = c(35, 53),
     cex.lab = 1.5)
abline(lm(m_dS ~ m_dL, data = boundary_stuff),
       lty = 2, lwd = 2, col = "darkgrey")
for (i in 1:length(colormap_list)) {
  add_image(recoloredImage(colormap_list[[i]]), 
            width = 4,
            x = boundary_stuff$m_dL[i],
            y = boundary_stuff$m_dS[i])
}

# my nemesis
saundersii <- "saundersii.png"
saun_1 <- recolorize2(saundersii, resize = 0.5, cutoff = 20)

plot(saun_1)

saun_2 <- absorbLayer(saun_1, 6, 
                      y_range = c(0, 0.8),
                      size_condition = function(s) s <= 1000)
saun_2 <- absorbLayer(saun_2, 2)

# this is literally the best I have ever gotten it:
saun_2 <- mergeLayers(saun_2, list(c(1, 3)))
