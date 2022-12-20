dist_triangular_ej <- function() {
    rj <- c(
        0.926, 0.0097, 0.1475, 0.2692, 0.6772, 0.05
    )
    ri <- c(0.843, 0.4655, 0.7284, 0.2795, 0.3236, 0.7602)

    final <- data.frame(rj, ri,
        "opcion 1" = ifelse(rj <= 0.33, 5 + (5 * sqrt(ri)), "-"),
        "opcion 2" = ifelse(rj > 0.33, 20 - (10) * sqrt(1 - ri), "-")
    )
    return(final)
}
# dist_triangular_ej()

random_rumbers <- function(ncol, nrow) {
    dt <- data.frame(matrix(ncol = ncol, nrow = nrow))
    for (nrow in 1:nrow) {
        for (ncol in 1:ncol) {
            dt[nrow, ncol] <- runif(1, 0, 0.6)
        }
    }
    return(dt)
}

dist_triangular <- function(i, min, moda, max) {
    comparacion <- (moda - min) / (max - min)
    r <- random_rumbers(2, i)
    r <- data.frame(r,
        "opcion 1" = ifelse(r$X1 <= comparacion, min + ((moda - min) * sqrt(r$X2)), "-"),
        "opcion 2" = ifelse(r$X1 > comparacion, max - (max - moda) * sqrt(1 - r$X2), "-")
    )
    return(r)
}

iteraciones <- as.numeric(readline("Ingrese el número de iteraciones: "))
min <- as.numeric(readline("Ingrese el valor minimo: "))
max <- as.numeric(readline("Ingrese el valor máximo: "))
moda <- as.numeric(readline("Ingrese la moda: "))

dist_triangular(iteraciones, min, moda, max)
# OPCION 1 = "b-(bc)*sqr(1-ri)" = ifelse(r2 < 0.33, "aceptado", "rechazado")
# OPCION 2 = a+(c+a)*sqr(ri)

