######################################
## kNN Example Forest CoverClassification based on Landsat composite image
######################################

options(stringsAsFactors=FALSE)
library(data.table)


FORESTED <- data.table(read.csv("R/forested.csv"))
COMPOSITE <- data.table(read.csv("R/composite.csv"))
MODIS <- data.table(read.csv("R/modis.csv"))

setnames(MODIS, "mean", "landcover")
setnames(FORESTED, "mean", "forestcover")

DF<- join(join(FORESTED[, c("system.index", "forestcover"), with=F],MODIS[, c("system.index", "landcover"), with=F]), COMPOSITE)

DF[, Forested := forestcover>.8]
DF[, MODISforested := (landcover>0 & landcover<=5)]

DF[, B3:=scale(B3)]
DF[, B4:=scale(B4)]
df.xlim <- range(DF$B3)
df.ylim<- range(DF$B4)

plot(DF[Forested==TRUE][1:120, c("B3", "B4"), with=F], col="green", xlim=df.xlim, ylim=df.ylim)
points(DF[Forested==FALSE][1:80, c("B3", "B4"), with=F], col="red", xlim=df.xlim, ylim=df.ylim)


set.seed(1151)
train<-sample(1:1000, 800)

# get the contour map

px1 <- range(DF[-train]$B3)
px1<-seq(px1[1], px1[2], 0.05)
px2 <- range(DF[-train]$B4)
px2<-seq(px2[1], px2[2],0.05)

xnew <- expand.grid(x1 = px1, x2 = px2)

knn15 <- knn(DF[train, c("B3","B4"), with=F], test = xnew, cl = DF[train]$Forested, k = 15, prob = TRUE)
prob <- attr(knn15, "prob")
prob <- ifelse(knn15==TRUE, prob, 1-prob)
prob15 <- matrix(prob, nrow = length(px1), ncol = length(px2))


par(mar = rep(2,4))
contour(px1, px2, prob15, levels=.5, labels="", xlab="", ylab="", main= "30-nearest neighbour", axes=FALSE)
points(DF[train,c("B3","B4"), with=F], col=ifelse(DF[train]$Forested==TRUE, "green", "red"))
points(xnew, pch=".", cex=3.5, col=ifelse(prob15>.5, "green", "red"))
box()




