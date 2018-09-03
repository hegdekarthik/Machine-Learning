#CLUSTERING
rm(list = ls())
#Hierarchial Clustering

distance = dist(iris[,-(5)], method = 'euclidean')
hier.cl = hclust(distance, method = 'ward.D')
plot(hier.cl)

ir.cluster = cutree(hier.cl,3)
rect.hclust(hier.cl,k=3, border = 'red')

table(iris$Species,ir.cluster)

## K-Means 
set.seed(1)
ir.fit = kmeans(iris[,-(5)],3)
ir.fit$cluster
table(iris$Species,ir.fit$cluster)
table(iris$Species)
iris$cluster = ir.fit$cluster

ir.fit$totss
ir.fit$withinss
ir.fit$tot.withinss
#Within Sum of squares
sl_c = mean(iris$Sepal.Lengt[iris$cluster == 1])
sw_c = mean(iris$Sepal.Width[iris$cluster == 1])
pl_c = mean(iris$Petal.Length[iris$cluster == 1])
pw_c = mean(iris$Petal.Width[iris$cluster == 1])

#withinss
sum((iris$Sepal.Length[iris$cluster == 1] - sl_c)^2 + (iris$Sepal.Width[iris$cluster == 1] - sw_c)^2 + (iris$Petal.Length[iris$cluster == 1] - pl_c)^2 + (iris$Petal.Width[iris$cluster == 1] - pw_c)^2)

#Centroid for length
l_cent = mean(iris$Sepal.Length)
w_cent = mean(iris$Sepal.Width)
pl_cen = mean(iris$Petal.Length)
pw_cen = mean(iris$Petal.Width)

#Total sum of squares
sum((iris$Sepal.Length - l_cent)^2 + (iris$Sepal.Width - w_cent)^2 + (iris$Petal.Length - pl_cen)^2 + (iris$Petal.Width - pw_cen)^2)

#Find value of k
tot_ss = c()

for (i in seq(1,4)){
  ir.fit_1 = kmeans(iris[,-(5)],i)
  tot_ss = c(tot_ss, ir.fit_1$tot.withinss)
  
}

plot(x=seq(1,4,1),y=tot_ss,type = 'l')
