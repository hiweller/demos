# packages we'll be using:
# install.packages(pkgs = c("ggplot2", "Rtsne", "umap", "plotly"))

# unpacking a PCA ####

# based on: http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf

# 1. format data
# first, we'll make up some data:
rawdata <- data.frame(a = c(2.5, 0.5, 2.2,
                            1.9, 3.1, 2.3,
                            2.0, 1.0, 1.5, 
                            1.1),
                      b = c(2.4, 0.7, 2.9, 
                            2.2, 3.0, 2.7,
                            1.6, 1.1, 1.6,
                            0.9))

# center the data
scaledata <- scale(rawdata, scale = FALSE)

# plot it
cols <- viridisLite::viridis(10)
plot(scaledata, col = cols,
     cex = 2, pch = 19, asp = 1,
     panel.first = c(abline(h = 0, 
                            v = 0, 
                            col = "grey")))

# 1. eigen decomposition
e <- eigen(cov(scaledata))
print(e)

# we can plot our eigenvectors right on our data:
# here's the first one (PC axis 1):
abline(b = e$vectors[1, 1] / e$vectors[2, 1],
       a = 0, lty = 2,
       col = "deeppink", lwd = 3)

# and the second:
abline(b = e$vectors[1, 2] / e$vectors[2, 2],
       a = 0, lty = 3,
       col = "lightpink", lwd = 3)
# they're orthogonal! (phew)

# pct variance explained
# values = std deviation, so we square to get variance, then 
# divide by their sum:
pctvar <- round(e$values^2 / sum(e$values^2) * 100, 1)

# 3. plot PC scores
pc_scores <- t(e$vectors) %*% t(scaledata)
pc_df <- data.frame(PC1 = pc_scores[1, ],
                    PC2 = pc_scores[2, ])

# by keeping the aspect ratio fixed, we can see this is essentially just
# rotating our original data so that the x-axis is the axis of greatest
# variation:
plot(pc_df, col = cols, pch = 19, cex = 2, asp = 1)
abline(h = 0, v = 0,
       col = c("deeppink", "lightpink"),
       lty = c(2, 3), lwd = 3)

# 4. reconstructing data from subset of PCs

# we only have two PCs here, so we'll use PC1 and leave out PC2
newdata <- scaledata %*% e$vectors
reconstruct <- newdata %*% matrix((e$vectors[1, ]))
rdf <- data.frame(a = reconstruct,
                  b = reconstruct)

# plot our original data again:
plot(scaledata, col = cols, 
     ylim = c(-1.5, 1.5),
     cex = 2, pch = 19, asp = 1,
     panel.first = c(abline(h = 0, 
                            v = 0, 
                            col = "grey")))

# and then our PC1-only reconstructed values:
points(rdf, col = "hotpink", cex = 2.1)

# ^ this is how you can get backtransforms for GMM!





# PCA vs. fancier dimension reduction techniques ####
swissroll <- readRDS("swissroll.RDS")
labels <- readRDS("swissroll_labels.RDS")
library(ggplot2)

# scale again!
swissroll <- scale(swissroll)

# colors
cols <- viridisLite::viridis(4)

# we have three dimensions this time, so we'll use plotly
plotly::plot_ly(x = swissroll[,1], 
                y = swissroll[,2], 
                z = swissroll[,3], 
                mode = "markers", color = factor(labels),
                colors = cols,
                alpha = 0.9, size = 1,
                type = "scatter3d")
# we have both large (spiral) and small (sub-group)-scale variation here

# we could plot all of our data using bivariate plots:
swissroll_df <- as.data.frame(swissroll)
colnames(swissroll_df) <- c("X", "Y", "Z")
plot(swissroll_df)
#...but that could get out of hand very quickly

# 1. using a PCA
pca <- prcomp(swissroll)
pca_pct <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
pca_df <- as.data.frame(pca$x)
pca_df$group <- factor(labels)

# sure enough, it goes for the axes of greatest variation:
ggplot(pca_df, aes(x = PC1, y = PC2)) + 
  geom_point(aes(color = group), alpha = 0.9) +
  scale_color_manual(values = cols) + 
  guides(color = FALSE) +
  coord_fixed() +
  xlab(paste0("PC1 (", pca_pct[1], "% var.)")) + 
  ylab(paste0("PC2 (", pca_pct[2], "% var.)")) + 
  theme_bw(base_size = 22) +
  theme(line = element_blank())

# we _kind_ of separate the groups in PC3:
ggplot(pca_df, aes(x = PC1, y = PC3)) + 
  geom_point(aes(color = group), alpha = 0.9) +
  scale_color_manual(values = cols) + 
  guides(color = FALSE) +
  coord_fixed() +
  xlab(paste0("PC1 (", pca_pct[1], "% var.)")) + 
  ylab(paste0("PC2 (", pca_pct[3], "% var.)")) + 
  theme_bw(base_size = 22) +
  theme(line = element_blank())

# 2. using tSNE
swissroll2 <- Rtsne::normalize_input(swissroll)
swissrollTsne <- Rtsne::Rtsne(swissroll2)
tsne_df <- as.data.frame(swissrollTsne$Y)
tsne_df$group <- factor(labels)

# downside of tSNE: it's stochastic (and non-linear)
# you will get different scores every time, and you
# can't get back your original data
ggplot(tsne_df, aes(x = V1, y = V2, color = group)) +
  geom_point() +
  scale_color_manual(values = cols) + 
  guides(color = FALSE) +
  theme_bw(base_size = 22) +
  theme(line = element_blank())
# ...but it's a useful visualization technique

# 3. UMAP
# newer, addresses some of the tSNE issues
# ...and usually overkill for comparative biologists!
swissroll_umap <- umap::umap(swissroll)
swissroll_umap <- as.data.frame(swissroll_umap$layout)
swissroll_umap$group <- factor(labels)

ggplot(swissroll_umap, aes(x = V1, y = V2, color = group)) +
  geom_point() +
  scale_color_manual(values = cols) + 
  guides(color = FALSE) +
  theme_bw(base_size = 22) +
  theme(line = element_blank())
