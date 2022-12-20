xi <- NULL
congruencial_mixto <- function(i, x, a, b, m) {
    for (i in i) {
        xn <- (a * x + b) %% m
        al <- xi / m

        xi <- c(xi, xn)
        x <- xn
    }

    data.frame(xi, "Aleatorio" = xi / m)
}



congruencial_mixto(1:20, 1, 6, 4, 13)
