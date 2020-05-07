load("cluster.RData")
# parameters "a" and "b" are two vectors 
# Manhattan distance is calculated between "a" and "b" when metric = "manhattan"
# otherwise Euclidean distance is calculated and returned by the function
my_dist_calculator <- function(a, b, metric = "euclidean") {
  if(metric == "manhattan") {
    mydistance <- 0
    for (i in 1:length(a)) {
      mydistance <- sum(mydistance + abs(a[i] - b[i]))
    }
  } else {
    mydistance <- 0
    for (i in 1:length(a)) {
      mydistance <- sum(mydistance + ((a[i] - b[i]) * (a[i]-b[i])))
    }
    mydistance <- sqrt(mydistance)
  }
  return(mydistance) 
}
#x is data, k is nb of cluster
k_means <- function(x, k, max.iter = 20){
  random_index <- sample(1:k, nrow(x), replace = TRUE)
  data_with_cluster <- cbind(x, clusterID = random_index)
  iterations = 1
  plot(data_with_cluster[,1:2])
  
  while (TRUE) {
    centroids <- matrix(rep(0, times = k * ncol(x)), nrow = k, ncol = ncol(x))
    for (i in 1:k) {
      obs_of_cluster_i <- which(data_with_cluster$clusterID == i);
      centroids[i,] <- colMeans(data_with_cluster[obs_of_cluster_i,][1:2]);
    }
    points(centroids[,1:2], pch = 20, cex = 2)
    dist_from_centroids <- matrix(rep(0, nrow(x) * k), nrow = nrow(x), ncol = k)
    for (i in 1:nrow(x)){
      for (j in 1:nrow(centroids)){
        dist_from_centroids[i,j] <- my_dist_calculator(centroids[j,], x[i,], "euclidean")
      }
    }
    obs_new_clusterID <- apply(dist_from_centroids, 1, which.min)
    if (all(obs_new_clusterID == data_with_cluster$clusterID)){
      km.clusters <- obs_new_clusterID
      centroid.matrix <- centroids
      break
    } else if (iterations > max.iter){
      break
    } else { 
      data_with_cluster$clusterID <- obs_new_clusterID
      iterations <- iterations + 1
    }
  }
  plot(data_with_cluster[,1:2], col = data_with_cluster$clusterID)
  points(centroid.matrix[,1:2], pch = 20, cex = 2, col = 1:k)
  return(list("clusters" = km.clusters, "centroids" = centroid.matrix))
}
elbow <- function() {
  within_ss <- numeric(7)
  for(k in 1:7){
    km.cl <- kmeans(cluster.data, k)
    within_ss[k] <- km.cl$tot.withinss
  }
  plot(x = 1:7, y = within_ss, type='b', xlab = "number of centers K", ylab = "total within SS" )
}
print(cluster.data)
km_clusters <- k_means(cluster.data, k = 3, max.iter = 15)
#elbow()