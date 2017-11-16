# ------------------------------------------------------------------------------
# CLEAN OBJECTS FROM WORKSPACE
# ------------------------------------------------------------------------------
rm(list=ls())

# ------------------------------------------------------------------------------
# LIBRARY
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# WORKING DIRECTORY
# ------------------------------------------------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 4/Clustering - Case Study 1/")



# ------------------------------------------------------------------------------
# SOURCE
# ------------------------------------------------------------------------------
source("myplclust.R")


# ------------------------------------------------------------------------------
# SLIGHTLY PROCESSED DATA
# ------------------------------------------------------------------------------
# - Download and Unzip Data
unzip("data.zip")
load("./data/samsungData.rda")
names(samsungData)[1:12]

# - Analisis by activities
table(samsungData$activity)



# ------------------------------------------------------------------------------
# FIST SUBJECT
# ------------------------------------------------------------------------------
samsungData <- transform(samsungData, activity=factor(activity))
sub1 <- subset(samsungData, subject==1)


# ------------------------------------------------------------------------------
# PLOTTING AVERAGE ACCELERATION FOR FIST SUBJECT
# ------------------------------------------------------------------------------
par(mfrow=c(1,3), mar=c(5,4,1,1))
plot(sub1[,1],col=sub1$activity,ylab=names(sub1)[1])
plot(sub1[,2],col=sub1$activity,ylab=names(sub1)[2])
plot(sub1[,3],col=sub1$activity,ylab=names(sub1)[3])
legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity),pch=1)


# ------------------------------------------------------------------------------
# CLUSTERING BASED JUST ON AVERAGE ACCELERATION FOR FIST SUBJECT
# ------------------------------------------------------------------------------
# - Reset par()
dev.off()
# - Hierarchical clustering - Continous Measure: Euclidean distance (linea recta)
distanceMatrix <- dist(sub1[,1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))
### Conclution: The dendrogram produced using the average acceleration features relatively
###             uninformative because average acceleration features do not appear to be able
###             to discriminate between the six different behaviors.



# ------------------------------------------------------------------------------
# PLOTTING MAX ACCELERATION FOR FIST SUBJECT
# ------------------------------------------------------------------------------
par(mfrow=c(1,3), mar=c(5,4,1,1))
plot(sub1[,10],col=sub1$activity,ylab=names(sub1)[10])
plot(sub1[,11],col=sub1$activity,ylab=names(sub1)[11])
plot(sub1[,12],col=sub1$activity,ylab=names(sub1)[12])
legend("topright",legend=unique(sub1$activity),col=unique(sub1$activity),pch=1)


# ------------------------------------------------------------------------------
# CLUSTERING BASED JUST ON  MAX ACCELERATION FOR FIST SUBJECT
# ------------------------------------------------------------------------------
# - Reset par()
dev.off()
# - Hierarchical clustering - Continous Measure: Euclidean distance (linea recta)
distanceMatrix <- dist(sub1[,10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))
legend("topleft",legend=unique(sub1$activity),col=unique(sub1$activity),pch=1)
### Conclution: There's two very clear clusters on the left hand side, you've got the,  kind
###             of the various walking activities. And on the right hand side you've got the 
###             various, you know, non moving activities. And so, beyond that, things are a 
###             little bit jumbled together, you can there's a lot of turquoise on the left 
###             and so that's. That's clearly one activity, but in the blue and the kind of 
###             magenta kind of mixed together. 
        



# ------------------------------------------------------------------------------
# SINGULAR VALUE DESCOMPOSITION
# ------------------------------------------------------------------------------
svd1 <- svd(scale(sub1[, -c(562,563)]))
par(mfrow=c(1,2), mar=c(5,4,1,1))
# - LEFT singular vector of X
plot(svd1$u[,1], col=sub1$activity)
plot(svd1$u[,2], col=sub1$activity)
### Conclution: And you can see, I'll take a look at the first and the second left singular vectors 
###             and color code them by activity. And again, you can kind of see there's a similar 
###             type of pattern. The first singular vector really seems to separate out the moving 
###             from the non moving. So you can see that there's a, a kind of a green, red, black 
###             on the bottom. And the blue, turquoise, magenta on the top. 
###             And then the sec, the second singular vector's a little bit somewhat a little bit  
###             more vague, what it's looking at. It seems to be separating out The magenta color  
###             from all the other clusters and so I think this is the walking down, or walking up  
###             one of those two. And so it's not clear what is different about that, that it kind  
###             of highlights, that gets highlighted on the second singular vector here. 


# ------------------------------------------------------------------------------
# FIND MAXIMUM CONTRIBUTER
# ------------------------------------------------------------------------------
# - Reset par()
dev.off()
# - RIGHT singular vector of X
plot(svd1$v[,2], col=sub1$activity)
### Conclution: In the second right singular vector (svd1$v[,2]) is kind of producing the most
###             variations between the different observations


# ------------------------------------------------------------------------------
# CLUSTERING WHITH MAXIMUM CONTRIBUTER
# ------------------------------------------------------------------------------
maxContrib <- which.max(svd1$v[,2])
distanceMatrix <- dist(sub1[,c(10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))
legend("topright",legend=unique(sub1$activity),col=unique(sub1$activity),pch=1)
### Conclution: Three movement activities clearly been separated
maxContrib_name <- names(samsungData)[maxContrib]



# ------------------------------------------------------------------------------
# K-MEANS CLUSTERING FOR FIST SUBJECT
# ------------------------------------------------------------------------------
# - Reset par()
dev.off()
kClust <- kmeans(sub1[,-c(562,563)], centers=6, nstart = 100)
table(kClust$cluster, sub1$activity)

# - Cluster 1 Variable Centers - Layind
plot(kClust$centers[1,1:10], pch=19, ylab="Cluster Center", xlab="")
# - Cluster 2 Variable Centers - Walking
plot(kClust$centers[4,1:10], pch=19, ylab="Cluster Center", xlab="")









