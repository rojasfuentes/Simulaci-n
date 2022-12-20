distribucion_erlang_ej <- function(i, k, m) {
    r1 <- c(0.9988, 0.0277, 0.5392, 0.3021, 0.5496, 0.3460)
    r2 <- c(0.6442, 0.0824, 0.4003, 0.8226, 0.5196, 0.2919)
    r3 <- c(0.9514, 0.1281, 0.4657, 0.2303, 0.6468, 0.1892)


    for (i in i) {
        pt <- -((m / k) * log(r1 * r2 * r3))
    }
    final <- data.frame(r1, r2, r3, "Tiempo Proceso min/pza" = pt)
    return(final)
}

distribucion_erlang <- function(i, k, m) {
    r1 <- runif(i, 0, 1)
    r2 <- runif(i, 0, 1)
    r3 <- runif(i, 0, 1)

    for (i in i) {
        pt <- -((m / k) * log(r1 * r2 * r3))
    }
    final <- data.frame(r1, r2, r3, "Tiempo Proceso min/pza" = pt)
    return(final)
}

distribucion_erlang_ej(1:6, 3, 8)
distribucion_erlang(1:10, 3, 8)
