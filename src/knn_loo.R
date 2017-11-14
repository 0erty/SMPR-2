petals <- iris[,3:4]
classes <- iris[,5]

# euclidian distances function
euclidean_distance <- function(u, v) {
  sqrt(sum((u-v)^2))
}

# use distfunction for our point(z) and every point in xl
dist_points <- function(xl, z, dist_function) {
  apply(xl, 1, dist_function, z)
}

# knn function return most popular class between first k distance
knn <- function(sort_distances, k) {
  return(names(which.max(table(names(sort_distances[1:k])))))
}


# loo function
loo <- function(points, classes) {
  size_y <- dim(points)[1]
  l <- rep(0, size_y-1)
  for (i in 1:size_y) {
    z <- points[i,]
    withoutz <- points[-i,]
    distances <- dist_points(points, z, euclidean_distance)[-i]
    names(distances) <- classes[-i]
    sort_distances <- sort(distances)
    for (k in 1:size_y-1) {
      l[k] <- l[k]+ifelse(knn(sort_distances, k) == classes[i], 0, 1)
    }
  }
  l <- l / size_y
  plot(1:length(l), l, type = "l")
  optimal_k <- which.min(l)
  points(optimal_k, l[optimal_k], pch = 20, col = "blue")
  return(optimal_k)
}

colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue")
par(mfrow = c(1, 2))
optimal_k <- loo(petals, classes)
plot(petals, bg = colors[iris$Species], pch = 21, asp = 1)
for (x in seq(1, 7, 0.1)) 
{
  for (y in seq(-1, 3, 0.1)) 
  {
    z <- c(x, y)
    distances <- dist_points(petals, z, euclidean_distance)
    names(distances) <- classes
    sort_distances <- sort(distances)
    bclass <- knn(sort_distances, optimal_k)
    # draw classified point
    points(z[1], z[2], col = colors[bclass], pch = 20) 
  }
}