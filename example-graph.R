# Make example-graph

M = matrix(data = c(0,1,1,1,1, rep(c(1,0,0,0,0), times=4)), nrow = 5)
M

library(igraph)

g = graph_from_adjacency_matrix(M, mode = 'undirected')
png(filename = "example-graph.png", width = 480, height = 480, res = 100)
plot(g, vertex.color = 'black', vertex.size = 8, vertex.label = NA)
dev.off()

N = matrix(data = c(0, 1, 2, 0, 1,
                    1, 0, 0, 1, 0,
                    2, 1, 0, 0, 0,
                    0, 0, 3, 0, 0,
                    3, 0, 0, 1, 0), nrow = 5)
N
g2 = graph_from_adjacency_matrix(N, mode = 'directed', weighted = T)
png(filename = "weighted-graph.png", width = 480, height = 480, res = 100)
plot(g2, vertex.color = 'black', vertex.size = 8, vertex.label = NA,
     edge.arrow.size = 1, edge.width = (E(g2)$weight)*2)
dev.off()
