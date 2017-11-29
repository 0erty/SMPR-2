library(ggplot2)
library(grid)
# функция расстояния, принимает на вход координаты двух точек
# и считает между ними расстояние
euclidean_distance <- function(u, v) {
  # корень из суммы квадрата разности элементов 'u' и 'v'
  sqrt(sum((u-v)^2))
}

plot.limits = function(arr, deviation = 0) {
    c(min(arr) - deviation, max(arr) + deviation)
}


#  функция применяющая функцию расстояния на все объекты выборки и заданную
# точку z
dist_points <- function(xl, z, dist_function) {
  # apply аналог функции map в python применяет функцию(dist_function) к выборке
  # xl, '1' означает построчное применение, '2' столбцовое, 'z' - доп параметр
  # для функции dist_function
  apply(xl, 1, dist_function, z)
}

# функция веса на основе ранга классов точек
weights_sum <- function(class, weights){
  sum(weights[names(weights) == class])
}



kernels.indicator = function(r) +(abs(r) <= 1)

# прямоугольное
kernels.Rectangle = function(r) {
    0.5 * kernels.indicator(r)
}

# треугольное
kernels.Triangle = function(r) {
    (1 - abs(r)) * kernels.indicator(r)
}

# биквадратичное
kernels.Biquadratic = function(r) {
    (15 / 16) * (1 - r^2)^2 * kernels.indicator(r)
}

# параболическое
kernels.Parabolic = function(r) {
    (3 / 4) * (1 - r^2) * kernels.indicator(r)
}

# триквадратичное 
kernels.Triquadratic = function(r) {
  (35 / 32) * (1 - r^2)^3 * kernels.indicator(r)
}

# Гаусовское
kernels.Gaus <- function(r){
  (1 / sqrt(2*pi)) * exp(1)^((-1/2)*r^2)
}

#ALL
kernels = list("G" = kernels.Gaus, "TQ" = kernels.Triquadratic, "R" = kernels.Rectangle, "T" = kernels.Triangle, "Q" = kernels.Parabolic, "E" = kernels.Biquadratic)

# ФУНКЦИЯ ДЛЯ GGPLOT2
# Фунуция для размещения нескольких графиков http://www.peterhaschke.com/r/2013/04/24/MultiPlot.html
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
