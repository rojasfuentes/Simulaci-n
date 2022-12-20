r1 <- c(
    0.6085, 0.4335, 0.1172, 0.7136, 0.5742,
    0.2299, 0.0312, 0.7517, 0.9599, 0.1770
)
r2 <- c(
    0.6156, 0.3291, 0.9653, 0.5558, 0.1912,
    0.4298, 0.9404, 0.6839, 0.2506, 0.5397
)
r3 <- c(
    0.3779, 0.1978, 0.9634, 0.4532, 0.2625,
    0.2807, 0.0190, 0.1421, 0.5739, 0.2798
)

dist_exponencial <- function(r, media) {
    for (i in seq_along(r)) {
        v_tiempollegada[i] <- -media * (log(1 - r1[i]))
    }
    return(v_tiempollegada)
}

v_tiempollegada <- numeric()
v_horallegada <- numeric()
v_tipo <- vector()
suma <- 0
v_gasolina <- numeric()

# Calculamos tiempo entre llegadas
v_tiempollegada <- dist_exponencial(r1, 6)

# Calculamos hora de llegada
for (i in seq_along(v_tiempollegada)) {
    suma <- suma + v_tiempollegada[i]
    v_horallegada[i] <- suma
}

# Calculamos el tipo de gasolina
for (i in seq_along(r2)) {
    if (r2[i] <= 0.2) {
        v_tipo[i] <- "90 Octanos"
    } else if (r2[i] > 0.2 && r2[i] <= 0.65) {
        v_tipo[i] <- "95 Octanos"
    } else if (r2[i] > 0.65 && r2[i] <= 1) {
        v_tipo[i] <- "97 Octanos"
    }
}

# Calculamos el volumen de gasolina
for (i in seq_along(r3)) {
    if (r3[i] <= 0.2) {
        v_gasolina[i] <- 2 + (15 - 2) * r3[i]
    } else if (r3[i] > 0.2 && r3[i] <= 0.65) {
        v_gasolina[i] <- 5 + (10 - 5) * r3[i]
    } else if (r3[i] > 0.65 && r3[i] <= 1) {
        v_gasolina[i] <- 8 + (15 - 8) * r3[i]
    }
}
final <- data.frame(
    "Ri" = r1,
    "Tiempo entre llegadas" = v_tiempollegada,
    "Hora de llegada" = v_horallegada,
    "R2" = r2,
    "Tipo de gasolina" = v_tipo,
    "R3" = r3,
    "Volumen de gasolina" = v_gasolina
)
final