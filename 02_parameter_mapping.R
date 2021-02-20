# define a function for calculating image residuals from a recolorize object
imResiduals <- function(recolorize_obj) {
  imDist(recolorize:::raster_to_array(recolorize_obj$original_img),
         recoloredImage(recolorize_obj), plotting = F)
}

# get our package images
images <- dir(system.file("extdata", package = "recolorize"), 
              "png", full.names = TRUE)

# fits recolorize objects for a range of cutoffs
recolorizeRange <- function(image, resize = 1,
                            cutoffs = seq(10, 120, by = 10)) {
  
  # outputs: residuals, fits, images, and number of colors
  res <- vector("numeric", length (cutoffs))
  fits <- vector("list", length(cutoffs))
  ims <- vector("list", length(cutoffs))
  ncolors <- vector("numeric", length(cutoffs))
  
  # fit one recolorize object for every cutoff
  for (i in 1:length(cutoffs)) {
    ful <- recolorize::recolorize2(image, bins = 3, resize = resize,
                                   cutoff = cutoffs[i], plotting = F)
    fits[[i]] <- ful
    ims[[i]] <- as.raster(recoloredImage(ful))
    res[i] <- sum(imResiduals(ful)^2, na.rm = TRUE)
    ncolors[i] <- nrow(ful$centers)
  }
  
  # normalize the residuals
  res <- res / max(res)
  
  # plot residuals as a function of cutoff
  par(mar = c(5, 5, 1, 1))
  plot(cutoffs, res, xlab = "CIE Lab cutoff", ylab = "Sum of sq. residuals",
       pch = 19, type = "b", cex = 2, cex.lab = 1.5,
       ylim = c(0, 1.1),
       xlim = c(-5, 105))
  for (i in 1:length(cutoffs)) {
    add_image(ims[[i]], cutoffs[i], res[i], width = 10)
  }
  
  # plot residuals as a function of n_colors
  plot(ncolors, res, xlab = "Number of colors", ylab = "Sum of sq. residuals",
       pch = 19, type = "b", cex = 2, cex.lab = 1.5,
       ylim = c(0, 1.1))
  for (i in 1:length(cutoffs)) {
    add_image(ims[[i]], ncolors[i], res[i], width = 2)
  }
  
  # return
  return(list(residuals = res,
              fits = fits,
              images = ims,
              ncolors = ncolors))
}


# tests out range of cutoffs on all of the images 
for (i in 1:length(images)) {
  im1 <- recolorizeRange(images[i])
  adj_list <- lapply(im1$fits, recolorize_adjacency)
  for (i in 1:length(adj_list)) {
    if (i == 1) {
      adj_df <- adj_list[[i]][-grep("n_off|p_|q_|t_|B|Rt|Rab", names(adj_list[[i]]))]
    } else {
      adj_df <- rbind(adj_df,
                      adj_list[[i]][-grep("n_off|p_|q_|t_|B|Rt|Rab", names(adj_list[[i]]))])
    }
  }
  
  if (any(adj_df$k <= 2)) {
    adj_df <- adj_df[-which(adj_df$k <= 2), ]
  }
  pca <- prcomp(adj_df)
  plot(pca$x, asp = 1)
  for (i in 1:nrow(adj_df)) {
    add_image(im1$images[[i]], pca$x[i, 1], pca$x[i, 2], width = 5)
  }
  readline("Press [enter] to continue\n")
}
