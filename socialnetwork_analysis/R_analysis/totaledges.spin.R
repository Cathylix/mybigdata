edge0 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/0.edges", header = F)
edge107 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/107.edges", header = F)
edge348 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/348.edges", header = F)
edge414 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/414.edges", header = F)
edge686 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/686.edges", header = F)
edge698 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/698.edges", header = F)
edge1684 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/1684.edges", header = F)
edge1912 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/1912.edges", header = F)
edge3437 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/3437.edges", header = F)
edge3980 <- read.table("/Users/Cathy/Desktop/Courses/Big Data/project1/facebook/3980.edges", header = F)

totaledges <- rbind(edge0,edge107)
totaledges <- rbind(totaledges,edge348,edge414,edge686,edge698,edge1684,edge1912,edge3437,edge3980)

g <- graph.data.frame(totaledges)
g <-simplify(g)
plot(g)
hist(degree(g))

locs <- layout.fruchterman.reingold(g)
plot(g, layout=locs, vertex.label=NA, main="Original",vertex.color=degree(g))
vertex.color=V(g)$color

degree(g)

subg <- subgraph.edges(g, E(g)[patent %in% patents[okpatents]])

#extract subgraph
subg <- subgraph.edges(g, E(g)[patent %in% patents[okpatents]])

#verify
plot(gs, edge.label=E(subg)$patent)


