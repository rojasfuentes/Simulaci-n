dist_exponencial <- function(r, media) {
    for (i in seq_along(r)) {
        v_tiempollegada[i] <- -media * (log(1 - r1[i]))
    }
    return(v_tiempollegada)
}
dist_uniforme <- function(r, a, b) {
    for (i in seq_along(r)) {
        v_tiempodepositos[i] <- a + (b - a) * r[i]
    }
    return(v_tiempodepositos)
}
# crea una funcion que calcule la distribucion normal
dist_normal <- function(r1, r2, media, desviacion) {
    for (i in seq_along(r1)) {
        v_tiempotransferencia[i] <- media + desviacion * sqrt(-2 * log(r1[i])) * cos(2 * pi * r2[i])
    }
    return(v_tiempotransferencia)
}
# Variables aleatorias
r1 <- c(
    0.3792, 0.5022, 0.7695, 0.7073, 0.3563,
    0.2871, 0.5578, 0.1543, 0.9694, 0.7695
)
r2 <- c(
    0.2438, 0.8770, 0.4501, 0.8656, 0.6516,
    0.4576, 0.3510, 0.9585, 0.2699, 0.3964
)
r3 <- c(
    0.8711, 0.7112, 0.1050, 0.3532, 0.0757,
    0.1209, 0.8824, 0.0565, 0.9432, 0.0999
)
r4 <- c(
    0.6929, 0.1781, 0.1800, 0.6330, 0.7838,
    0.0848, 0.5235, 0.4611, 0.9109, 0.5738
)
r5 <- c(
    0.2024, 0.0006, 0.2593, 0.2328, 0.3442,
    0.6050, 0.2861, 0.3539, 0.6549, 0.1524
)
r6 <- c(
    0.1262, 0.7632, 0.3092, 0.8544, 0.6363,
    0.4606, 0.6001, 0.8288, 0.6346, 0.4739
)

# r1 Tiempo entre llegadas | Dist exponencial con media 3.5
# r2 Tipo de transaccion | Retiro 40% | Transferencias 35% | Depositos 25%
# r3 Tiempo de depositos | Dist uniforme entre 9 y 12
# r4 Tiempo de transferencias | Dist normal con media 12 y desviacion 1.5
# r5 R2 para la distribucion normal
# r6 Tiempo de retiros | Dist exponencial con media 2.5

# crea una funcion que calcule la distribucion exponencial


# vectores vacios
v_tiempollegada <- numeric()
v_tipo <- vector()
v_tiempodepositos <- numeric()
v_tiempotransferencia <- numeric()
v_tiemporetiro <- numeric()
v_tiempotransaccion <- numeric()

# Calculamos tiempo entre llegadas
v_tiempollegada <- dist_exponencial(r1, 3.5)
v_tiempollegada

# Calculamos el tipo de transaccion

for (i in seq_along(r2)) {
    if (r2[i] <= 0.4) {
        v_tipo[i] <- "Retiro"
    } else if (r2[i] > 0.4 && r2[i] <= 0.75) {
        v_tipo[i] <- "Transferencia"
    } else if (r2[i] > 0.75 && r2[i] <= 1) {
        v_tipo[i] <- "Deposito"
    }
}
v_tipo
# Calculamos el tiempo de depositos
v_tiempodepositos <- dist_uniforme(r3, 9, 12)
# Calculamos el tiempo de transferencias
v_tiempotransferencia <- dist_normal(r4, r5, 12, 1.5)
# Calculamos el tiempo de retiros
v_tiemporetiro <- dist_exponencial(r6, 2.5)

#Calculamos el tiempo de transaccion

for (i in seq_along(v_tipo)) {
    if (v_tipo[i] == "Retiro") {
        v_tiempodepositos[i] <- NA
        v_tiempotransferencia[i] <- NA
        v_tiempotransaccion[i] <- v_tiemporetiro[i]
    } else if (v_tipo[i] == "Transferencia") {
        v_tiemporetiro[i] <- NA
        v_tiempodepositos[i] <- NA
        v_tiempotransaccion[i] <- v_tiempotransferencia[i]
    } else if (v_tipo[i] == "Deposito") {
        v_tiemporetiro[i] <- NA
        v_tiempotransferencia[i] <- NA
        v_tiempotransaccion[i] <- v_tiempodepositos[i]
    }
}

# Creamos el data frame
df <- data.frame(
    "Tiempo de llegada" = v_tiempollegada,
    "Tipo de transacción" = v_tipo,
    "Tiempo de transacción" = v_tiempotransaccion,
    v_tiempodepositos,
    v_tiempotransferencia,
    v_tiemporetiro
)
df
