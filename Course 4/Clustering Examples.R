set.seed(1234)
par(mar=c(6,6,6,2))

x <- rnorm(12,mean=rep(1:3,each=4),sd=.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05, labels=as.character(1:12))

dataFrame <- data.frame(x=x,y=y)

################## Hierarchical clustering ################## 
# Continous Measure: Euclidean distance (linea recta)
# - Calcular las distancias entre los nodos
distxy <- dist(dataFrame)

# - Method of visualizing: Plot
hclustering <- hclust(distxy)
plot(hclustering)

# - Method of visualizing: Heat Maps
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)


################## K-Means clustering ################## 
# - Method of visualizing: Plot
kmeansObj <- kmeans(dataFrame, centers=3)
plot(x,y,col=kmeansObj$cluster, pch=17, cex=2)
points(kmeansObj$centers, col=1:3, pch=3, cex=3, lwd=3)

# Plot 4
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
kmeansObj <- kmeans(dataMatrix, centers=3)
par(mfrow=c(1,2), mar=c(2,4,.1,.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt="n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt="n")
