dist_exponencial <- function(r, media) {
    df <- data.frame("Ri" = r)
    df$Tiempo_entre_llegadas <- -media * (log(1 - r))
    return(df)
}

dist_normal <- function(i, media, des_es) {
    for (i in 1:i) {
        v_distnorm[i] <- (sum(r1[i]) - (media / 2)) * des_es + media
    }
    return(v_distnorm)
}
# Variables globales
r1 <- c(
    0.6653, 0.3516, 0.9979, 0.3936, 0.0614,
    0.3328, 0.4694, 0.3821, 0.6172, 0.5135
)
r2 <- c(
    0.3802, 0.0816, 0.7757, 0.5085, 0.9400,
    0.3392, 0.4709, 0.6732, 0.6203, 0.9954
)

v_tiempollegada <- numeric()
suma <- 0
v_distnorm <- numeric()
v_inicioinspeccion <- numeric()
v_fininspeccion <- numeric()
v_tiempoeninspeccion <- numeric()
v_tiempopromedio <- numeric()
v_tiempoespera <- numeric()
suma_promedio <- 0

# Inicio
# Calculamos tiempo entre llegadas
df2 <- dist_exponencial(r1, 5)
df2

# Calculamos tiempo de llegada
for (i in seq_along(df2$Tiempo_entre_llegadas)) {
    suma <- suma + df2$Tiempo_entre_llegadas[i]
    v_tiempollegada[i] <- suma
}
v_tiempollegada

# Calculamos tiempo de inspección
v_distnorm <- dist_normal(length(r1), 4, 0.5)
v_distnorm


# Calculamos Inicio de la inspección y Fin de la Inspección
v_inicioinspeccion[1] <- v_tiempollegada[1]
for (i in seq_along(df2$Tiempo_entre_llegadas)) {
    v_fininspeccion[i] <- v_inicioinspeccion[i] + v_distnorm[i]
    if (i < length(df2$Tiempo_entre_llegadas)) {
        v_inicioinspeccion[i + 1] <-
            max(v_tiempollegada[i + 1], v_fininspeccion[i])
    }
}

v_inicioinspeccion
v_fininspeccion

# Calculamos tiempo en inmstrucción
v_tiempoeninspeccion <- v_fininspeccion - v_tiempollegada
v_tiempoeninspeccion

#cálculo del tiempo de espera
v_tiempoespera <- v_inicioinspeccion - v_tiempollegada
v_tiempoespera

# Calculamos tiempo promedio de inspeccion
for (i in seq_along(df2$Tiempo_entre_llegadas)) {
    suma_promedio <- suma_promedio + v_tiempoeninspeccion[i]
    v_tiempopromedio[i] <- suma_promedio / i
}

v_tiempopromedio

# ordenamos los datos en una tabla
final <- data.frame(
    "Pieza" = r1,
    "Tiempo entre llegadas" = df2$Tiempo_entre_llegadas,
    "Tiempo de llegada" = v_tiempollegada,
    "Inicio de la inspección" = v_inicioinspeccion,
    "Tiempo de inspección" = v_distnorm,
    "Fin de la inspección" = v_fininspeccion,
    "Tiempo en inspección" = v_tiempoeninspeccion,
    "Tiempo en espera" = v_tiempoespera,
    "Tiempo promedio de inspección" = v_tiempopromedio
)
final
