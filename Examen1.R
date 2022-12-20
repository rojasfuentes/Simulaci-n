# var globales
ri <- c(
    0.8459, 0.1841, 0.5695, 0.2109, 0.5362, 0.1377,
    0.5684, 0.6989, 0.5831, 0.1002
)
v <- vector()

# Criterios de aceptacion
for (i in seq_along(ri)) {
    if (ri[i] <= 0.15) {
        v[i] <- "90 Octanos"
    } else if (ri[i] > 0.15 && ri[i] <= 0.8) {
        v[i] <- "95 Octanos"
    } else if (ri[i] > 0.8 && ri[i] <= 1) {
        v[i] <- "97 Octanos"
    }
}

# Resultado
final <- data.frame(
    "No Aleatorio " = ri,
    "Criterio" = v
)

final
