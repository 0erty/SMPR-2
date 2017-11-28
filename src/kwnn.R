source("functions.R")
petals <- iris[,3:4]
classes <- iris[,5]


get_weight <- function(i, k) {
  return ((k + 1 - i) / k)
}

kwnn <- function(sorted_distances, k) {
  k_distances <- sorted_distances[1:k]
  weights <-get_weight(1:k, k)
  names(weights) <- names(k_distances)
  weights <- sapply(unique(names(weights)), weights_sum, weights)
  return(names(which.max(weights)))
}

kwnn_loo <- function(points, classes) {
  size_y <- dim(points)[1]
  l <- rep(0, size_y-1)
  for (i in 1:size_y) {
    z <- points[i,]
    distances <- dist_points(points, z, euclidean_distance)[-i]
    names(distances) <- classes[-i]
    sort_distances <- sort(distances)
    for (k in 1:size_y-1) {
      l[k] <- l[k]+ifelse(kwnn(sort_distances, k) == classes[i], 0, 1)
    }
  }
  l <- l / size_y
  plot(1:length(l), l, type = "l", main="LOO")
  optimal_k <- which.min(l)
  points(optimal_k, l[optimal_k], pch = 19, col = "blue")
  text(optimal_k, l[optimal_k]+0.1, labels = paste("K=", optimal_k, " | LOO = ", round(l[optimal_k], 2)), pos = 4, col = "blue")
  return(optimal_k)
}

colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue")
par(mfrow = c(1, 2))
optimal_k <- kwnn_loo(petals, classes)
plot(petals, bg = colors[iris$Species], pch = 21, asp = 1, main="Optimal KWNN")
for (x in seq(1, 7, 0.1))
{
  for (y in seq(-1, 3, 0.1))
  {
    z <- c(x, y)
    distances <- dist_points(petals, z, euclidean_distance)
    names(distances) <- classes
    sort_distances <- sort(distances)
    bclass <- kwnn(sort_distances, optimal_k)
    # draw classified point
    points(z[1], z[2], col = colors[bclass], pch = 20)
  }
}
