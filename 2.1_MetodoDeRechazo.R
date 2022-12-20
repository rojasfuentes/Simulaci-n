# i = numero de iteraciones # m = media
# a = limite inferior # b = limite superior

metodo_de_rechazo_ejemplo <- function(i, a, b, m) {
    r1 <- c(0.6088, 0.1931)
    r2 <- c(0.1931, 0.7971)

    for (i in i) {
        x1 <- a + (b - a) * r1
        fx1 <- ((1 / sqrt(2 * pi)) * exp(-((x1^2) / 2)))
        fx1m <- fx1 / m
    }
    final <- data.frame(r1, r2, x1, fx1m,
        "criterio" = ifelse(r2 < fx1m, x1, NA),
        "estado" = ifelse(r2 < fx1m, "aceptado", "rechazado")
    )
    return(final)
}


metodo_de_rechazo <- function(i, a, b, m) {
    r1 <- runif(i, 0, 1)
    r2 <- runif(i, 0, 1)

    for (i in i) {
        x1 <- a + (b - a) * r1
        fx1 <- ((1 / sqrt(2 * pi)) * exp(-((x1^2) / 2)))
        fx1m <- fx1 / m
    }
    final <- data.frame(r1, r2, x1, fx1m,
        "criterio" = ifelse(r2 < fx1m, x1, NA),
        "estado" = ifelse(r2 < fx1m, "aceptado", "rechazado")
    )
    return(final)
}

metodo_de_rechazo_ejemplo(1:2, 3, -3, 0.4)
metodo_de_rechazo(1:10, 3, -3, 0.4)
