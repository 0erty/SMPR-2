source("functions.R")
# цвета классов
colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue")

# параметры объектов выборки
petals <- iris[, 3:4]

# рисуем ирисы фишера
plot(petals, pch=21, bg=colors[iris$Species], col=colors[iris$Species], asp=1)

# функция knn с k = 1
knn_1 <- function(z) {
  #  функция применяющая функцию расстояния на все объекты выборки и заданную
  # точку z
  distances <- dist_points(petals, z, euclidean_distance)
  # выбираем индекс минимального элемента dist_points
  min_index <- which(distances == min(distances))
  # возвращаем класс
  return(iris[min_index, 5])
}

# цикл по горизонтали
for (x in seq(1, 7, 0.1))
{
  # цикл по вертикали
  for (y in seq(-1, 3, 0.1))
  {
    # классифицируемая точка
    z = c(x, y)
    # исключаем классификацию точек выборки
    if (any(sapply(points, function(v) all(v == z)))) next
    # применяем knn и определяем класс
    bclass = knn_1(z)
    # рисуем точку
    points(z[1], z[2], col = colors[bclass], pch = 20)
  }
}
