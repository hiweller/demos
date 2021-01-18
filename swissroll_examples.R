# bookkeeping ####
library(ggplot2)
library(Rtsne)
library(umap)
library(plotly)
load("03_PCA/swissroll.Rdata")
load("03_PCA/swissroll_labels.Rdata")
swissroll <- scale(swissroll)
# colors
cols <- viridisLite::viridis(4)

# plot raw data
plotly::plot_ly(x = swissroll[,1], 
                y = swissroll[,2], 
                z = swissroll[,3], 
                mode = "markers", color = factor(labels),
                colors = cols,
                alpha = 0.9, size = 1,
                type = "scatter3d")

# bivariate plots ####
swissroll.df <- as.data.frame(swissroll)
colnames(swissroll.df) <- c("X", "Y", "Z")
png("00_images/03a_swissroll_plotmatrix.png",
    width = 5, height = 5, units = "in", res = 600)
plot(swissroll.df)
dev.off()


# PCA ####
pca <- prcomp(swissroll)
pca.pct <- round(pca$sdev^2 / sum(pca$sdev^2) * 100, 1)
pca.df <- as.data.frame(pca$x)
pca.df$group <- factor(labels)
g <- ggplot(pca.df, aes(x = PC1, y = PC2)) + 
  geom_point(aes(color = group), alpha = 0.9) +
  scale_color_manual(values = cols) + 
  guides(color = FALSE) +
  coord_fixed() +
  xlab(paste0("PC1 (", pca.pct[1], "% var.)")) + 
  ylab(paste0("PC2 (", pca.pct[2], "% var.)")) + 
  theme_bw(base_size = 22) +
  theme(line = element_blank()); g
ggsave("00_images/03b_swissroll_PC12.png",
       dpi = 600, width = 5, height = 5)

g <- ggplot(pca.df, aes(x = PC1, y = PC3)) + 
  geom_point(aes(color = group), alpha = 0.9) +
  scale_color_manual(values = cols) + 
  guides(color = FALSE) +
  xlab(paste0("PC1 (", pca.pct[1], "% var.)")) + 
  ylab(paste0("PC3 (", pca.pct[3], "% var.)")) + 
  theme_bw(base_size = 22) +
  theme(line = element_blank()); g
ggsave("00_images/03b_swissroll_PC13.png",
       dpi = 600, width = 5, height = 5)

g <- ggplot(pca.df, aes(x = PC2, y = PC3)) + 
  geom_point(aes(color = group), alpha = 0.9) +
  scale_color_manual(values = cols) + 
  guides(color = FALSE) +
  xlab(paste0("PC2 (", pca.pct[2], "% var.)")) + 
  ylab(paste0("PC3 (", pca.pct[3], "% var.)")) + 
  theme_bw(base_size = 22) +
  theme(line = element_blank()); g
ggsave("00_images/03c_swissroll_PC23.png",
       dpi = 600, width = 5, height = 5)

vardf <- data.frame(PC = colnames(pca$rotation),
                    pctvar = pca.pct)

pv <- ggplot(vardf, aes(x = PC, y = pctvar)) +
  geom_col() +
  xlab("Principal component") + ylab("% variance explained") +
  theme_bw(base_size = 22) + 
  theme(line = element_blank())

ggsave("00_images/03g_pctvar.png", plot = pv,
       dpi = 600, width = 5, height = 5)
# tSNE ####
swissroll2 <- Rtsne::normalize_input(swissroll)
swissrollTsne <- Rtsne::Rtsne(swissroll2)
tsne.df <- as.data.frame(swissrollTsne$Y)
tsne.df$group <- factor(labels)

g2 <- ggplot(tsne.df, aes(x = V1, y = V2, color = group)) +
  geom_point() +
  scale_color_manual(values = cols) + 
  guides(color = FALSE) +
  theme_bw(base_size = 22) +
  theme(line = element_blank()); g2
ggsave("00_images/03c_swissrollTsne.png",
       g2, 
       dpi = 600, width = 5, height = 5)

# UMAP ####
swissroll.umap <- umap(swissroll)
swissroll.umap <- as.data.frame(swissroll.umap$layout)
swissroll.umap$group <- factor(labels)

g3 <- ggplot(swissroll.umap, aes(x = V1, y = V2, color = group)) +
  geom_point() +
  scale_color_manual(values = cols) + 
  guides(color = FALSE) +
  theme_bw(base_size = 22) +
  theme(line = element_blank()); g3
ggsave("00_images/03c_swissrollUMAP.png",
       g3, 
       dpi = 600, width = 5, height = 5)
