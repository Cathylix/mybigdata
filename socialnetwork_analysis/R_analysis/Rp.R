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


totaledges <- rbind(edge0,edge107,edge348,edge414,edge686,edge698,edge1684,edge1912,edge3437,edge3980)
g <- graph.data.frame(totaledges)
g <-simplify(g)
plot(g)

#Function 1 Degree and degree distribution of the vertices
hist(degree(g))
plot(degree.distribution(g))

#Function 2 Delete vertices
degree300 <- delete.vertices(g,V(g)[degree(g)<300])
plot(degree300)
degree400 <- delete.vertices(g,V(g)[degree(g)<400])
plot(degree400, layout = layout.fruchterman.reingold)

#Function 3 Use layout format
locs <- layout.fruchterman.reingold(degree400)
plot(degree400, layout=locs, vertex.label=NA, main="Original",vertex.color=degree(degree400))
vertex.color=V(g)$color

#Function 4 Generate subgraph
subdegree400 <- subgraph.edges(degree400, 1:5) 
plot(subdegree400)
subdegree400 <- subgraph.edges(degree400, 1:10) 
plot(subdegree400)
subdegree400 <- subgraph.edges(degree400, 1:20)
plot(subdegree400)

#Function 5 Set vertice attribute
V(subdegree400)$color <- sample( c("red", "black"), vcount(subdegree400), rep=TRUE)
E(subdegree400)$color <- "grey"
red <- V(subdegree400)[ color == "red" ]
black <- V(subdegree400)[ color == "black" ]
E(subdegree400)[ red %--% red ]$color <- "red"
E(subdegree400)[ black %--% black ]$color <- "black"
$
#Function 6 Change size of vertices
subdegree400sizechange <- subdegree400
V(subdegree400sizechange)$size <- degree(subdegree400sizechange)*2
plot(subdegree400sizechange, layout = layout.fruchterman.reingold)

#Function 7 Set Edge attribute
E(subdegree400)$weight <- runif(ecount(subdegree400))
E(subdegree400)$color <- "grey"
E(subdegree400)[ weight > 0.9 ]$color <- "red"
plot(subdegree400, vertex.size=12, layout=layout.kamada.kawai,edge.width=0.2+1*E(subdegree400)$weight)

#Function 8 Betweenness
head(betweenness(subdegree400),50)
sub2degree400 <- subgraph.edges(degree400, 1:10)
head(betweenness(sub2degree400),50)
head(betweenness(degree400),50)

#Function 9 Closeness
head(closeness(subdegree400),50)
sub2degree400 <- subgraph.edges(degree400, 1:10)
head(closeness(sub2degree400),50)
head(closeness(degree400),50)

#Function 10 Average path
average.path.length(subdegree400)
average.path.length(sub2degree400)
average.path.length(degree400)

#Function 11 Cliques
head(cliques(degree400),10)
head(cliques(subdegree400),10)
head(cliques(sub2degree400),10)

clu <- components(g)
> groups(clu)


