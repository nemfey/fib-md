#HIERARCHICAL CLUSTERING

#Select all variables and all obs.
library("fpc")
library(clusterSim)
df$WhichCompany<-as.factor(df$WhichCompany)
dissimMatrix <- daisy(df, metric = "gower", stand=TRUE)
distMatrix<-dissimMatrix^2
h1 <- hclust(distMatrix,method="ward.D")  # NOTICE THE COST
plot(h1)
lbl_clust <- cutree(h1, 5)
#round(calinhara(distMatrix,lbl_clust, cn=max(lbl_clust)),digits=2)
rect.hclust(h1, k=5, border=2:7)
table(cutree(h1,5))




#Sampling with 50 observations
set.seed(123456)
sam<-as.vector(sort(sample(1:nrow(df),50)))
data_num_sam <- df[sam,]
dissimMatrix2 <- daisy(data_num_sam, metric = "gower", stand=TRUE)
distMatrix2<-dissimMatrix2^2
h2 <- hclust(distMatrix2,method="ward.D")  # NOTICE THE COST
plot(h2)
lbl_clust_sam <- cutree(h2, 5)
round(calinhara(distMatrix2,lbl_clust_sam),digits=2)
rect.hclust(h2, k=5, border=2:7)
table(cutree(h2,5))


#K-means

#For all numerical data
set.seed(696969)
aux<-c(3,4,5,7,8,11,13,14,16)
dcon <- df[,aux]
fviz_nbclust(dcon, kmeans, method = "wss")

k2 <- kmeans(dcon,4)
library(factoextra)
bank.clusters <- k2$cluster
fviz_cluster(list(data=dcon, cluster=bank.clusters),geom = "point", ggtheme = theme_bw())
round(calinhara(dcon,bank.clusters, cn=max(bank.clusters)),digits=2)
table(bank.clusters)


#For a small sample of 50 obs
sam<-as.vector(sort(sample(1:nrow(df),50)))
dcon_sam <- dcon[sam,]
dist_mat <- dist(dcon_sam)
k4 <- kmeans(dcon_sam,4)
bank.clusters <- k4$cluster
fviz_cluster(list(data=dcon_sam, cluster=bank.clusters),geom = "point", ggtheme = theme_bw())
round(calinhara(dist_mat,bank.clusters, cn=max(bank.clusters)),digits=2)


#Profiling of clusters
df$Cluster <- lbl_clust
boxplot(df$Term ~df$Cluster)

