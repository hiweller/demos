# bookkeeping ####
library(ggplot2)

install.packages(pkgs = c("ggplot2", "Rtsne", "umap", "plotly"))

# format data ####
rawdata <- data.frame(a = c(2.5, 0.5, 2.2,
                            1.9, 3.1, 2.3,
                            2.0, 1.0, 1.5, 
                            1.1),
                      b = c(2.4, 0.7, 2.9, 
                            2.2, 3.0, 2.7,
                            1.6, 1.1, 1.6,
                            0.9))

# make color vector
cols <- RColorBrewer::brewer.pal(10, "Spectral")
cols <- viridisLite::viridis(10)

# center
scaledata <- scale(rawdata, scale = FALSE)

# make a dataframe for plotting
scaledf <- data.frame(scaledata, id = factor(1:10))

# eigen decomposition ####
e <- eigen(cov(scaledata))

# plot it
dataplot <- ggplot(scaledf,
       aes(x = a, y = b)) +
  coord_fixed() +
  xlim(c(-1.5, 1.5)) + ylim(c(-1.5, 1.5)) +
  geom_vline(xintercept = 0, col = "lightgrey") +
  geom_hline(yintercept = 0, col = "lightgrey") +
  geom_point(aes(color = id), size = 6) + 
  scale_color_manual(values = cols) +
  guides(color = FALSE) +
  theme_bw(base_size = 22) +
  theme(line = element_blank()); dataplot

ggsave("00_images/03d_pca_rawdata.png", plot = dataplot, 
       dpi = 600, 
       width = 5, height = 5)

# with eigenvectors
evec1 <- dataplot + geom_abline(slope = c(e$vectors[1, 1] / e$vectors[2, 1]),
            intercept = 0,
            lty = 2, lwd = 1.5,
            col = "violetred1"); evec1

evec2 <- evec1 + geom_abline(slope = e$vectors[1, 2] / e$vectors[2, 2],
                               intercept = 0,
                               lty = 3, lwd = 1.5,
                               col = "orchid1"); evec2
ggsave("00_images/03e_pca_eigenvector1.png", plot = evec1, 
       dpi = 600, 
       width = 5, height = 5)
ggsave("00_images/03e_pca_eigenvector2.png", plot = evec2, 
       dpi = 600, 
       width = 5, height = 5)


# generate new data ####
newdata <- t(e$vectors) %*% t(scaledata)
newdf <- data.frame(PC1 = newdata[1, ],
                    PC2 = newdata[2, ], 
                    id = factor(1:10))

# pct variance explained
pctvar <- round(e$values^2 / sum(e$values^2) * 100, 1)

# plot it
pcplot <- ggplot(newdf,
       aes(x = PC1, y = PC2)) +
  coord_fixed() + 
  geom_vline(xintercept = 0, col = "lightgrey") +
  geom_hline(yintercept = 0, col = "lightgrey") +
  xlim(c(-2, 2)) + ylim(c(-2, 2)) +
  xlab(paste0("PC1 (", pctvar[1], "% var.)")) +
  ylab(paste0("PC2 (", pctvar[2], "% var.)")) +
  geom_point(aes(color = id), size = 6) + 
  scale_color_manual(values = cols) +
  guides(color = FALSE) +
  theme_bw(base_size = 22) +
  theme(line = element_blank()); pcplot
ggsave("00_images/03f_pcplot.png", 
       plot = pcplot, 
       dpi = 600, 
       width = 5, height = 5)
ggsave("00_images/03f_pcplot_lines.png", 
       plot = pcplot + geom_hline(yintercept = 0,
                                  lty = 2, lwd = 1.5,
                                  col = "violetred1") +
         geom_vline(xintercept = 0,
                    lty = 3, lwd = 1.5,
                    col = "orchid1"), 
       dpi = 600, 
       width = 5, height = 5)

# transform original data ####
newdata <- scaledata %*% e$vectors
oldata <- newdata %*% t(e$vectors)
reconstruct <- newdata %*% matrix((e$vectors[1, ]))
rdf <- data.frame(a = reconstruct,
                  b = reconstruct)

dicols <- c(cols, rep("violetred1", 10))
rplot <- ggplot(scaledf,
                 aes(x = a, y = b)) +
  coord_fixed() + 
  geom_vline(xintercept = 0, col = "lightgrey") +
  geom_hline(yintercept = 0, col = "lightgrey") +
  xlim(c(-2, 2)) + ylim(c(-2, 2)) +
  xlab("a") +
  ylab("b") +
  #geom_point(aes(color = id), size = 5) +
  geom_point(data = rdf, aes(x = a, y = b), 
             shape = 21, size = 6,
             col = "violetred1", fill = alpha("violetred1", 0.3)) +
scale_color_manual(values = cols) +
  guides(color = FALSE) +
  theme_bw(base_size = 22) +
  theme(line = element_blank()); rplot
ggsave("00_images/03i_reconstruct.png", 
       plot = rplot, 
       dpi = 600, 
       width = 5, height = 5)
ggsave("00_images/03j_reconstruct2.png", 
       plot = rplot + geom_point(aes(color = id), size = 5), 
       dpi = 600, 
       width = 5, height = 5)

test <- newdata %*% matrix(e$vectors[1, ], ncol = 1)
plot(test, test)
test <- t(newdata) %*% t(e$vectors)
plot(test)  
matrix(e$vectors[1, ], ncol = 1)

plot(t(test), t(test), asp = 1)
points(scaledata, col = "red")

plot(scaledata)
t(test)
scaledata
