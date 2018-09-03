rm(list = ls())

#Read the data 
movie_data = read.table("Documents/PGP-DSE/movie_dataset.txt", header = FALSE, sep = "|",fill = TRUE, quote = "\"")
colnames(movie_data) = c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure","Animation","Children","Comedy","Crime","Documentary","Drama","Fantasy","FlimNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")
View(movie_data)
table(movie_data$Thriller)

#Check for NA and empty
sapply(movie_data, function(x){sum(is.na(x))})
sapply(movie_data, function(x){sum(x == "")})

#Get unique rows
movie_update = unique(movie_data)

#Get the rows which has dupliacte entries
which(duplicated(movie_data$Title))
movie_data[duplicated(movie_data$Title),]
movie_data[movie_data$Title == 'Chasing Amy (1997)',]

#Do the clustering process
movie_required = movie_update[,6:ncol(movie_update)]

distance_k = dist(movie_required, method = 'euclidean')
mov.cl = hclust(distance_k, method = 'ward.D')
plot(mov.cl)

movie.cluster = cutree(mov.cl,6)
rect.hclust(mov.cl,k=6, border = 'red')
table(movie.cluster)

#Update new column
movie_update$cluster = movie.cluster

#Check the number 
sapply(movie_update[,6:ncol(movie_update)],function(x){tapply(x,movie_update$cluster,mean)})

#Check the % for particular type of movie
tapply(movie_update$Action,movie.cluster,mean)*100

#Write it to csv
write.csv(movie_update[,c("Title","cluster")], "movie_cluster.csv", row.names = T)
sum(tapply(movie_update$Action,movie.cluster,mean)*100)

## K-Means 
set.seed(1)
mov.fit = kmeans(movie_required,10, nstart = 10, iter.max = 10)
mov.fit$cluster

#SOFT CLUSTERING METHOD
#Fuzzy C means
library(factoextra)
fviz_silhouette()

## Fuzzy C means for Soft Clustering
movie.dist = dist(movie_required)
library(cluster)
FANNY = fanny(as.matrix(movie.dist),k=10,maxit=2000)
names(FANNY)
head(FANNY$membership)
table(FANNY$clustering)
plot(FANNY)

View(movie_required)
