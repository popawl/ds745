library(ergm)
library(igraph)
library(intergraph)

# Load data
df <- read.csv('el.csv', header=TRUE, fileEncoding="UTF-8-BOM")
g <- graph.data.frame(df, directed=FALSE)
V(g)$type <- V(g)$name %in% df[,2]
pr <- bipartite.projection(g)
pr1 <- pr$proj1

# Load attributes. I am loading from
# csv and then creating a named list 
# to pass into vertex_attr
attr.df <- read.csv('attr.csv', header=TRUE, fileEncoding="UTF-8-BOM", stringsAsFactors=FALSE)
attr.df <- attr.df[3:ncol(attr.df)]
attr <- list()
for (i in 1:ncol(attr.df)) {
  attr <- append(attr, list(attr.df[,i]))
}
names(attr) <- colnames(attr.df)
vertex_attr(pr1) <- attr
pr1 <- set_vertex_attr(pr1, "relocation_id", value=unique(df$relocation_id))
pr1 <- delete.vertices(pr1, which(degree(pr1) == 0)) #delete isolates

# Plot
colors <- c("blue", "red")
shapes <- c("circle", "square")
plot(g, vertex.shape=shapes[V(g)$type+1], 
     vertex.color=colors[V(g)$type+1], vertex.size=4, 
     vertex.label=NA)
title("Point C Network",cex.main=1,col.main="black")
plot(pr1, vertex.color="blue", vertex.size=4, edge.width=E(pr1)$weight, vertex.label=NA, layout=layout_with_drl)
title("TRE Projection Network",cex.main=1,col.main="blue")

# Summary
#get.adjacency(pr1, sparse=FALSE, attr="weight")
gsize(pr1)
graph.density(pr1)
diameter(pr1)
components(pr1)
centr_eigen(pr1)$centralization

# Community
cw <- cluster_walktrap(pr1)
modularity(cw)
#slow
#ceb <- cluster_edge_betweenness(pr1)
#modularity(ceb)
cfg <- cluster_fast_greedy(pr1)
modularity(cfg)
clp <- cluster_label_prop(pr1)
modularity(clp)
cle <- cluster_leading_eigen(pr1)
modularity(cle)
cl <- cluster_louvain(pr1)
modularity(cl)
plot(cl, pr1, vertex.label=NA, vertex.size=4, layout=layout_with_drl)
table(V(pr1)$level, membership(cl))

# Model
pr1n <- asNetwork(pr1)
null.model <- ergm(pr1n ~ edges)
all.model <- ergm(pr1n ~ edges + nodematch("vehicle") + nodematch("child") + 
                  nodematch("spouse") + nodematch("parent") + nodematch("homeowner") + 
                  nodematch("homebuyer") + nodematch("level") + nodematch("pet") + 
                  nodematch("from_state") + nodematch("to_state"))
some.model <- ergm(pr1n ~ edges + nodematch("child", diff=TRUE) + 
                   nodematch("spouse", diff=TRUE) + nodematch("homeowner", diff=TRUE) + 
                   nodematch("homebuyer", diff=TRUE) + nodematch("level", diff=TRUE))
#Save vertices and edges
#write.csv(as_data_frame(pr1, what="vertices"), file="pr1_v.csv")
#write.csv(as_data_frame(pr1, what="edges"), file="pr1_e.csv")
