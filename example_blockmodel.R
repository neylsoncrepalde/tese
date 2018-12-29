# Exemplo blockmodel

library(sand)
library(igraph)
library(intergraph)
library(sna)

lazega = upgrade_graph(lazega)
is_directed(lazega)
laz = asNetwork(lazega)
laz %v% "vertex.names" = 1:36

se = sedist(laz, method = "correlation")
ec = equiv.clust(laz, equiv.dist = se, method = "correlation")
bm = blockmodel(laz, ec, k = 4)
dplyr::glimpse(bm)
bm$blocked.data
plot(bm)
summary(bm)

my_plotbm = function(x, ...) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    n <- dim(x$blocked.data)[2]
    m <- stackcount(x$blocked.data)
    if (!is.null(x$plabels)) 
      plab <- x$plabels
    else plab <- (1:n)[x$order.vector]
    if (!is.null(x$glabels)) 
      glab <- x$glabels
    else glab <- 1:m
    #par(mfrow = c(floor(sqrt(m)), ceiling(m/floor(sqrt(m)))))
    if (m > 1) 
      for (i in 1:m) {
        plot.sociomatrix(x$blocked.data[i, , ], main = "", drawlab = T, diaglab = F,
                         drawlines = F, cex.lab = .5)
        for (j in 2:n) if (x$block.membership[j] != x$block.membership[j - 
                                                                       1]) 
          abline(v = j - 0.5, h = j - 0.5, lty = 3)
      }
    else {
      plot.sociomatrix(x$blocked.data, main = "", drawlines = F, drawlab = T, diaglab = F,
                       cex.lab = .5)
      for (j in 2:n) if (x$block.membership[j] != x$block.membership[j - 
                                                                     1]) 
        abline(v = j - 0.5, h = j - 0.5, lty = 3)
    }
}

png(filename = "example_blockedmatrix.png", width = 480, height = 480, res = 100)
my_plotbm(bm)
dev.off()

png(filename = "example_sociomatrix.png", width = 480, height = 480, res = 100)
plot.sociomatrix(laz, main = "", drawlines = T, drawlab = T, diaglab = F,
cex.lab = .5)
dev.off()

png(filename = "example_socio_and_blockedmatrix.png", width = 960, height = 480, res = 100)
par(mfrow = c(1,2))
plot.sociomatrix(laz, main = "", drawlines = T, drawlab = T, diaglab = F, cex.lab = .5)
my_plotbm(bm)
dev.off()