library("igraph")
library("plyr")
# Load and normalize data
d <- read.csv("~/Documents/dm_survey_data/2016_dm_survey_tip_bigrams.csv")

# This removes any connections with fewer than min_connections
min_connections <- 5
d <- data.frame(count(d))
d <- d[d["freq"] >= min_connections,]

# Build a graph from the data
g <- graph_from_data_frame(data.frame(d["X0"],d["X1"]),directed = FALSE)

# Simplify by removing duplicate edges and loops
g <- simplify(g, remove.loops = TRUE, remove.multiple = TRUE)

# Narrow down on the primary connections and ignore outlying clusters.
#g <- induced.subgraph(g, which(membership(clusters(g)) == which.max(sizes(clusters(g)))))

# Build list based on degree
deg <- as.data.frame(degree(g))

# How many nodes and edges?
print(length(V(g)))
print(length(E(g)))

# Generate a nice layout. Takes a long time for a larger number of edges
layout <- layout_with_fr(g, grid=c("nogrid"))
layout <- norm_coords(layout, ymin=-1, ymax=1, xmin=-1, xmax=1)

# Find communities using walktrap.
comm <- cluster_walktrap(g)

# Generate a color pallate for communities and add them to vectors and edges.
colors <- rainbow(max(membership(comm)), start = 0, end = 1, alpha = .8)
V(g)$color <- colors[membership(comm)]
edge.start <- ends(g, es=E(g), names=F)[,1]
edge.col <- V(g)$color[edge.start]

# Display the size of the label based on total degrees.
V(g)$degree <- degree(g)
V(g)$label.cex <- 1.5 * V(g)$degree / max(V(g)$degree)+ 1

# Generate a png output.
png("~/Documents/dm_survey_data/2016_dm_survey_word_graph.png", height=5000, width=5000, bg = "black",pointsize = 50)
par(mar=c(.1, .1, .1, .1))
plot.igraph(g, rescale=F, layout=layout, vertex.size=.1, 
            #vertex.shape="circle", vertex.label.color= "#ffffff", 
            vertex.shape="none", vertex.label.color= colors[membership(comm)], 
            vertex.frame.color=colors[membership(comm)],
            edge.curved=.5,edge.arrow.size=0,
            edge.color=edge.col,
            #edge.color="#aaaaff90",
            vertex.label = ifelse(degree(g) > 0, V(g)$name, NA))
dev.off()
