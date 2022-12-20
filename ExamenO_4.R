dist_exponencial <- function(r, media) {
    for (i in seq_along(r)) {
        v_tiempollegada[i] <- -media * (log(1 - r[i]))
    }
    return(v_tiempollegada)
}
r1 <- c(
    0.2612, 0.7108, 0.9602, 0.9785, 0.5237,
    0.5216, 0.0118, 0.8153, 0.0670, 0.5988
)
r2 <- c(
    0.6067, 0.5353, 0.4941, 0.3886, 0.7451,
    0.2556, 0.9074, 0.2012, 0.7981, 0.9044
)
r3 <- c(
    0.0154, 0.8033, 0.2603, 0.1598, 0.2913,
    0.9541, 0.2135, 0.3748, 0.0981, 0.2054
)
r4 <- c(
    0.0656, 0.3375, 0.5922, 0.1951, 0.1660,
    0.7178, 0.5980, 0.0771, 0.8140, 0.2908
)
r5 <- c(
    0.2942, 0.9883, 0.7408, 0.2408, 0.5023,
    0.9557, 0.0313, 0.8126, 0.8001, 0.6948
)
r6 <- c(
    0.2849, 0.2203, 0.4828, 0.6433, 0.9315,
    0.9675, 0.2755, 0.0425, 0.6382, 0.3168
)

r <- c(r1, r2, r3, r4, r5, r6)
random <- runif(length(r), 0, 1)
v_tiempollegada <- numeric()
v_operacion <- numeric()
v_servicio <- numeric()

# Calculamos el tiempo de llegada
v_tiempollegada <- dist_exponencial(r, 4)

# Indicamos el tipo de operacion
for (i in seq_along(random)) {
    if (random[i] <= 0.5) {
        v_operacion[i] <- "Retiro"
    } else if (random[i] > 0.5 && random[i] <= 0.8) {
        v_operacion[i] <- "Transferencia"
    } else if (random[i] > 0.8 && random[i] <= 1) {
        v_operacion[i] <- "Deposito"
    }
}

# Calculamos el tiempo de servicio
for (i in seq_along(random)) {
    if (random[i] <= 0.5) {
        v_servicio[i] <- 2 + (4 - 2) * random[i]
    } else if (random[i] > 0.5 && random[i] <= 0.8) {
        v_servicio[i] <- -3 * (log(1 - random[i]))
    } else if (random[i] > 0.8 && random[i] <= 1) {
        v_servicio[i] <- (random[i] - (3 / 2)) * 0.2 + 3
    }
}
random
final <- data.frame(r,
    "Tiempo de llegada" = v_tiempollegada,
    "Operacion" = v_operacion,
    "Tiempo de servicio" = v_servicio
)
final
