######################################
## Support Vector Illustrations  
######################################

options(stringsAsFactors=FALSE)
library(data.table)

set.seed(2014)

library(dplyr)
TRUECENTERS <-data.frame( x1=c(3.5, -1), x2=c(-2, 2))
centers <- data.frame(cluster=as.numeric(1:2), size=c(50, 50), x1=TRUECENTERS[,1], x2=TRUECENTERS[,2])
pts <- centers %>% group_by(cluster) %>%
    do(data.frame(x1=rnorm(.$size[1], .$x1[1]),
                  x2=rnorm(.$size[1], .$x2[1])))


##different lines
plot(x2 ~ x1,  pch=cluster+3, col=as.factor(cluster), data=pts, cex=1)
abline(a=-1.25, b=1)
abline(a=-1.7, b=1.5)
abline(a=-2.25, b=.75)
abline(a=-2, b=1,lw=5)


library(e1071)
svmfit=svm(factor(cluster) ~ x2 + x1, data=pts, kernel="linear", cost=10,scale=FALSE)
plot(svmfit, pts)

