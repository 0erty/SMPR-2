source("functions.R")




parzen_window <- function(distances, classes, z, h){
    distances <- distances / h

    distances = mc.PW.kernel(distances) #???????????????

    weights <- sapply(unqie(classes), weights_sum, distances)
    names(weights) <- unique(classes)
    return(names(which.max(weights)))
}


loo <- function(points, classes, hLimits) {
    size_y <- dim(points)[1]
    l <- rep(0, length(hLimits))

    for (i in 1:size_y) {
        z <- points[i,]
        distances <- dist_points(points, z, euclidean_distance)[-i]
        names(distances) <- classes[i]

        for(j in 1:length(hLimits)) {
            h <- hLimits[j]
            bclass <- parzen_window(distances, classes[-i], z, h)
            l[j] <- l[j] + ifelse(bclass == classes[i], 0, 1)
        }
    }
    return(l / size_y)
}









petals <- iris[, 3:4]
classes <- iris[, 5]

par(xpd=NA)
