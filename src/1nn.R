# colors
iris_colors <- c("red", "green3", "blue")
# names
iris_names <- c("setosa", "versicolor", "virginica")
# petal.Length && petal.Width
petals <- iris[, 3:4]
# colors : names 
names(iris_colors) <- iris_names 
# draw 
plot(petals, pch=21, bg=iris_colors[iris$Species], col=iris_colors[iris$Species], asp=1)

# euclidian distances function
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

# 1NN algorithm
knn_1 <- function(z) {
  # apply like map function in python, use euclidianDistance between every petal string and our point z
  irisDist <- apply(petals, 1, FUN=euclideanDistance, z) 
  min_index<- which(irisDist == min(irisDist))
  return(iris[min_index, 5])
}

for (x in seq(1, 7, 0.1)) 
  {
  for (y in seq(-1, 3, 0.1)) 
    {
    # classified point
    z = c(x, y)
    # classified point
    class = knn_1(z)
    # draw classified point
    points(z[1], z[2], col = iris_colors[class], pch = 20) 
  }
}





