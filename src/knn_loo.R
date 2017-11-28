source("functions.R")
petals <- iris[,3:4]
classes <- iris[,5]

# функция классификации выбирает возвращает класс который встречается максимальное количество раз среди первых k
# отсортированных дистанций
knn <- function(sort_distances, k) {
  return(names(which.max(table(names(sort_distances[1:k])))))
}

# функция скользящего контроля для определения оптимального k, классифицирует каждую точку выборки подставляя разные k
loo <- function(points, classes) {
  # dim - определяет размерность матрицы (x,y), [1] - количество строк(150)
  size_y <- dim(points)[1]
  # rep(x,y) повторяет x y раз, следовательно создаем список из 149 элементов
  # 149 так как loo проверяет каждый элемент выборки следовательно один занят
  l <- rep(0, size_y-1)

  for (i in 1:size_y) {
    # z проверяемая точка из списка объектов
    z <- points[i,]
    # дистанции между объектами и проверяемой точкой
    distances <- dist_points(points, z, euclidean_distance)[-i]
    # присваеваем классы дистанциям
    names(distances) <- classes[-i]
    # сортировка дистанций от меньшего к большему
    sort_distances <- sort(distances)
    # проверяем количество ошибок классификации
    for (k in 1:size_y-1) {
      # если классифицированный элемент совпадает с реальным классом то оставляем 0,
      # иначе прибавляем 1
      l[k] <- l[k]+ifelse(knn(sort_distances, k) == classes[i], 0, 1)
    }
  }
  # делим l на 150 для удобства отображения на графике
  l <- l / size_y
  # определяем оптимальный k
  optimal_k <- which.min(l)
  # отрисовка LOO
  plot(1:length(l), l, type = "l", main="LOO")
  # отрисовка точки k на графике loo
  points(optimal_k, l[optimal_k], pch = 20, col = "red")
  text(optimal_k, l[optimal_k]+0.1, labels = paste("K=", optimal_k, " | LOO = ", round(l[optimal_k], 2)), pos = 4, col = "black")
  # возвращаем оптимальный k для дальнейшего построения карты классификации
  return(optimal_k)
}
# цвета классов
colors <- c("setosa" = "red", "versicolor" = "green3","virginica" = "blue")
# функция для отображения нескольких графиков на одной странице
par(mfrow = c(1, 2))
# вызов функции loo, определение оптимального k
optimal_k <- loo(petals, classes)
# отрисовка стандартной выборки ирисов Фишера
plot(petals, bg = colors[iris$Species], pch = 21, asp = 1, main="Optimal KNN")

# построение карты классификации
# цикл для горизонтали
for (x in seq(1, 7, 0.1))
{
  # цикл для вертикали
  for (y in seq(-1, 3, 0.1))
  {
    # классифицируемая точка
    z <- c(x, y)
    if (any(sapply(points, function(v) all(v == z)))) next
    # дистанции между объектами и проверяемой точкой
    distances <- dist_points(petals, z, euclidean_distance)
    # присваеваем классы дистанциям
    names(distances) <- classes
    # сортировка дистанций от меньшего к большему
    sort_distances <- sort(distances)
    # определяем класс точки
    bclass <- knn(sort_distances, optimal_k)
    # рисуем классифицированную точку
    points(z[1], z[2], col = colors[bclass], pch = 20)
  }
}
