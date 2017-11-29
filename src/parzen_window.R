#help functions
source("functions.R")

# глобальная переменная для ядра
parzen_kernel <- NULL #use this kernel

# функция парзеновского окна
parzen_window <- function(distances, classes, h) {
  # делим дистанции на ширину окна по формуле
  distances <- distances / h
  # передаем их в ядро
  distances <- parzen_kernel(distances)
  classes <- unique(classes)
  # суммируем каждый вес
  weights <- sapply(classes, weights_sum, distances)
  # присваиваем имена
  names(weights) <- classes
  # при невозможности классифицировать из-за отсутствия соседей в радиусе возвращаем
  # unknown class
  if (max(weights) == 0) return("unknown class")
  
  # возвращаем именя класс с максимальным весом
  return(names(which.max(weights)))
}

# loo для определения оптимальной ширины окна
loo_parzen_window <- function(points, classes, hl) {
  # размерность выборки
  size_y <- dim(points)[1]
  # массив для подсчета ошибок
  l <- rep(0, length(hl))
  
  # цико от 1 до 150 для классификации каждой точки и проверки
  for (i in 1:size_y) {
    # классифицируемая точки
    z <- points[i,]
    # находим дистанции между точкой и выборкой
    distances <- dist_points(points[-i,], z, euclidean_distance)
    # определяем класс каждой дистанции для удобства
    names(distances) <- classes[-i]
    
    # перебор разных h для проверки
    for (j in 1:length(hl)) {
      # проверяемы h
      h <- hl[j]
      # опредеяем класс при данном h
      b_class <- parzen_window(distances, classes[-i], h)
      
      l[j] <- l[j] + ifelse(b_class == classes[i], 0, 1)
    }
  }
  return(l / size_y)
}

# отрисовку loo пришлось вынести в отдельную функцю так как parzen_kernel нужно
# до его вызова
draw_loo <- function(points, classes, hl) {
  # список h
  hs <- NULL
  # список ошибок
  loo_list <- NULL
  
  for (kernel in names(kernels)) {
    # задаем ядро
    parzen_kernel <- kernels[[kernel]]
    # вызываем loo для каждого ядра, покажет нам, что ядро не особо влияет на результат
    l <- loo_parzen_window(points, classes, hl)
    # отрисовка графика
    plot(hl, l, type = "l", xlab = "h", ylab = "LOO")
    # определяем оптимальный h
    h_opt <- hl[which.min(l)]
    min_loo <- l[which.min(l)]
    # отрисовка оптимальной точки на графике loo
    points(h_opt, min_loo, pch = 21, col = "black")
    # label для точки
    text(h_opt, min_loo, labels = paste("h=", h_opt, pos = 3, col = "black", family = "mono")
         # список h
         hs <- c(hs, h_opt)
         loo_list <- c(loo_list, min_loo)
  }
  # выбираем лучшее ядро и ставим его для построения карты классификации
  parzen_kernel <- kernels[which.min(loo_list)]
  # возвращаем h
  return(hs[index])
}


draw_parzen_window <- function(points, classes, h, xlim, ylim, step) {
  # цвета
  colors = c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue", "unknown class" = "black")
  # отрисовка базового графика
  plot(points, bg = colors[classes], pch = 21, asp = 1, xlim = xlim, ylim = ylim, main = "парзеновское окно")
  
  for (x in seq(1, 7, 0.1)) {
    for (y in seq(-1, 3, 0.1)) {
      # точка
      z <- c(x,y)
      # не классифицировать точку из выборки
      if (any(apply(points, 1, function(v) all(v == z)))) next
      # расстояния
      distances <- dist_points(points, z, euclidean_distance)
      # присваеваем им классы
      names(distances) <- classes
      # находим оптимальный вариант
      b_class <- parzen_window(distances, classes, h)
      
      # рисуе точку на карте
      points(z[1], z[2], col = colors[b_class], pch = 21)
    }
  }
}

petals = iris[, 3:4]
classes = iris[, 5]
par(xpd = NA)
# карта отображения графиков на странице
layout(matrix(c(1, 2, 5, 5, 3, 4, 5, 5), 2, 4, byrow = T))
# лимиты h
hl <- seq(0.1, 2, 0.01)
# оптимаьлный h
h_opt = draw_loo(petals, classes, hl)
draw_parzen_window(petals, classes, h = h_opt, xlim = plot.limits(petals[, 1], 0.2), ylim = plot.limits(petals[, 2], 0.2), step = 0.1)
