

points.matrix <- cbind(x1 = points$x1, x2 = points$x2)
kclust <- kmeans(points.matrix, 3)
kclust

kclust$iter

###it runs three iteration, lets plot each iteration in turn

kclust <- kmeans(points[,c(2,3)], 3)
kclust$iter

##randomly select 3 points to begin
set.seed(1990)

plot(x2 ~ x1, col=as.factor(cluster), data=points, cex=.5)
points(x2 ~ x1, data=points[sample(1:nrow(points),3),], pch=4)
points(x2 ~ x1, data=TRUECENTERS, pch=2)

##setting same seed ensures that kmeans at first iteration starts with the same three points we selected above, go through loop step by step
set.seed(1990)
for(i in 1:3) {

kclust <- kmeans(points[,c(2,3)], 3, iter.max=i,nstart=1)
points$clusterk<- kclust$cluster 
points<-points[order(points$clusterk),]
plot(x2 ~ x1, col=as.factor(clusterk), data=points, cex=.5)
points(x2 ~ x1, data=kclust$centers, pch=4)
##plot true means as well
points(x2 ~ x1, data=kclust$centers, pch=4)
points(x2 ~ x1, data=TRUECENTERS, pch=2)

}

###how does total within cluster sum of squares evolve?


set.seed(1990)
TSS<-NULL
for(k in 1:10) {

kclust <- kmeans(points.matrix, k)
##plot the different clusters
plot(x2 ~ x1, col=as.factor(kclust$cluster), data=points, cex=.5)
TSS<-rbind(TSS,kclust$tot.withinss)
}

plot(TSS)