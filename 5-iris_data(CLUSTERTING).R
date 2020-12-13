# The iris data published by Fisher (1936) have been widely used for examplesin discriminant analysis 
# and cluster analysis. The sepal length, sepal width, petal length, and petal width are measured in millimeters 
# on 50 iris specimens from each of three species, Iris setosa, I. versicolor, and I. virginica.

#----------------------------------------------------------------------------------------------
# Hierarchical clustering
# (a) Use Ward's method to cluster the data set and interpret the results.

data = read.csv("C:\\Users\\khanal\\Desktop\\multivariate data analysis\\5-iris-data-cluster\\iris.txt",
                     dec = ".", sep = ",")

# summary of the data
head(data)
names(data)
class(data)
dim(data)
summary(data)
attributes(data)

d <- dist(data, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")


#----------------------------------------------------------------------------------------------
# Hierarchical clustering
#(b) Use the method average to cluster the data set and interpret the results.

d <- dist(data, method = "euclidean") # distance matrix
fit <- hclust(d, method="average")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")




#----------------------------------------------------------------------------------------------
# Hierarchical clustering
# (c) Use the method complete to cluster the data set and interpret the results.

d <- dist(data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")



#----------------------------------------------------------------------------------------------
# Hierarchical clustering
# (d) Use the method single to cluster the data set and interpret the results.

d <- dist(data, method = "euclidean") # distance matrix
fit <- hclust(d, method="single")
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")

#----------------------------------------------------------------------------------------------
# 2. (a) Use k-means clustering to cluster the iris data.


# K-Means Clustering with 3 clusters
head(data)
ir=data[,1:5]
head(ir)
head(data)

fit3 = kmeans(ir, 3)
fit3

# K-Means Clustering with 2 clusters
fit2 = kmeans(ir, 2)
fit2

# i think r2 value when cluster =3 is higher os cluster equal to 3 must be taken. 

# (b) Illustrate the results graphically.
library(cluster)
clusplot(ir, fit3$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

