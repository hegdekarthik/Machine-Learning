
#Clear the workspace
rm(list = ls())

## Importing packages
library(ggplot2)
library(cluster)
library(fpc)
library(factoextra)
library(tidyverse) # metapackage with lots of helpful functions

## Reading in files
movie_data = read.table("../input/movie_dataset.txt", header = FALSE, sep = "|",fill = TRUE, quote = "\"")

#Since it has no columns, enter the column names
colnames(movie_data) = c("ID","Title","ReleaseDate","VideoReleaseDate",
                        "IMDB","Unknown","Action","Adventure","Animation",
                        "Children","Comedy","Crime","Documentary","Drama",
                        "Fantasy","FlimNoir","Horror","Musical","Mystery",
                        "Romance","SciFi","Thriller","War","Western")
## Just a initial structure of the dataset
head(movie_data)

#Check for NA and empty
sapply(movie_data, function(x){sum(is.na(x))})
sapply(movie_data, function(x){sum(x == "")})

#Remove the duplicate rows from the data

#Get unique rows
movie_unique = unique(movie_data)

#Clustering of the data

#METHOD-1

#Hierarchical Clustering
movie_genres = movie_unique[,6:ncol(movie_unique)]

#Find the euclidean distance
cluster_distance = dist(movie_genres, method = 'euclidean')
mov.cl = hclust(cluster_distance, method = 'ward.D')

#Plot the cluster to decide how many number of division to make
plot(mov.cl)

#After looking into the plot I have deided to make 6 clusters. Let's see how does it look
movie.cluster = cutree(mov.cl,6)
#rect.hclust(mov.cl,k=6, border = 'red')

#Look at the categarization of all movies
table(movie.cluster)

#Update new column
movie_unique$cluster = movie.cluster

#Check the percentage of genre of movies which falls in each category/genre
sapply(movie_unique[,6:ncol(movie_unique)],function(x){tapply(x*100,movie_unique$cluster,mean)})

#Visualisations
plotcluster(movie_genres, movie_unique$cluster)


#Write it to csv to have a broader look of data
write.csv(movie_unique[,c("Title","cluster")], "movie_hierarchical_cluster.csv", row.names = T)


#METHOD-2

#K-means clustering

#Since we can't decide the value of k directly, let's look into total.withinss of each clusters and the elbow plot
#and then let's decide the value of k.
set.seed(1)
tot_ss = c()

#Run a for loop to get the best value of k
for (i in seq(1,10,1)){
  mov.fit_temp = kmeans(movie_genres,i)
  tot_ss = c(tot_ss, mov.fit_temp$tot.withinss)
  
}

#Plot the elbow curve
plot(x=seq(1,10,1),y=tot_ss,type = 'l')

#Perform K-means Clustering with the value of k=6
mov.fit = kmeans(movie_genres,6, nstart = 8, iter.max = 10)
mov.fit$cluster

#Since we already have the Cluster column, overwrite the same column to update new value of cluster.
movie_unique$cluster = NULL
movie_unique$cluster = mov.fit$cluster


# Again check the percentage of genre of movies which falls under respective cluster
sapply(movie_unique[,6:ncol(movie_unique)],function(x){tapply(x*100,movie_unique$cluster,mean)})

#Visualisation
#Silouhette
sil <- silhouette(mov.fit$cluster, dist(movie_genres))
fviz_silhouette(sil)

#Write the output to a CSV file to check the clean data
write.csv(movie_unique[,c("Title","cluster")], "movie_kmeans_cluster.csv", row.names = T)

