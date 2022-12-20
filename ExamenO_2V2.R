dist_exponencial <- function(r, media) {
    df <- data.frame("Ri" = r)
    df$Tiempo_entre_llegadas <- -media * (log(1 - r))
    return(df)
}

dist_uniforme <- function(i, ri, a, b) {
    for (i in 1:i) {
        x1 <- a + (b - a) * ri
        v_uniforme <- x1
    }
    return(v_uniforme)
}

dist_normal <- function(i, media, des_es) {
    for (i in 1:i) {
        v_distnorm[i] <- (sum(r[i]) - (media / 2)) * des_es + media
    }
    return(v_distnorm)
}
# Variables globales
r1 <- c(
    0.7017, 0.9901, 0.9211, 0.0585, 0.7398,
    0.3780, 0.9452, 0.1867, 0.8779, 0.0599
)
r2 <- c(
    0.2496, 0.3988, 0.1811, 0.1881, 0.6004,
    0.3424, 0.0726, 0.1873, 0.9414, 0.5060
)
r3 <- c(
    0.8544, 0.3364, 0.0406, 0.6679, 0.9603,
    0.4442, 0.4738, 0.8787, 0.3263, 0.6528
)
r4 <- c(
    0.0169, 0.1967, 0.0345, 0.2651, 0.5156,
    0.3464, 0.9380, 0.2522, 0.0753, 0.1888
)
r <- c(r1, r2, r3, r4)

v_nopieza <- 1:length(r)
v_uniforme <- numeric()
v_tiempollegada <- numeric()
suma <- 0
v_distnorm <- numeric()
v_inicioinspeccion <- numeric()
v_fininspeccion <- numeric()
v_tiempoeninspeccion <- numeric()
v_tiempopromedio <- numeric()
v_tiempoespera <- numeric()
v_operario <- vector()
suma_promedio <- 0

# Inicio
# Calculamos tiempo entre llegadas
v_uniforme <- dist_uniforme(length(r), r, 5, 10)

# Calculamos tiempo de llegada
for (i in seq_along(r)) {
    suma <- suma + v_uniforme[i]
    v_tiempollegada[i] <- suma
}
v_tiempollegada

# Calculamos tiempo de inspección
v_distnorm <- dist_normal(length(r), 4, 0.5)
v_distnorm


# Calculamos Inicio de la inspección y Fin de la Inspección
v_inicioinspeccion[1] <- v_tiempollegada[1]
for (i in seq_along(r)) {
    v_fininspeccion[i] <- v_inicioinspeccion[i] + v_distnorm[i]
    if (i < length(r)) {
        v_inicioinspeccion[i + 1] <-
            max(v_tiempollegada[i + 1], v_fininspeccion[i])
    }
}
# Indicamos el operario
for (i in seq_along(v_nopieza)) {
    if (i <= (length(r) * 0.32)) {
        v_operario[i] <- "Operario 1"
    } else if (i > (length(r) * 0.32) && i <= (length(r) * 0.65)) {
        v_operario[i] <- "Operario 2"
    } else if (i > (length(r) * 0.65) && i <= length(r)) {
        v_operario[i] <- "Operario 3"
    }
}

v_inicioinspeccion
v_fininspeccion

# Calculamos tiempo en inmstrucción
v_tiempoeninspeccion <- v_fininspeccion - v_tiempollegada
v_tiempoeninspeccion

# cálculo del tiempo de espera
v_tiempoespera <- v_inicioinspeccion - v_tiempollegada
v_tiempoespera

# Calculamos tiempo promedio de inspeccion
for (i in seq_along(r)) {
    suma_promedio <- suma_promedio + v_tiempoeninspeccion[i]
    v_tiempopromedio[i] <- suma_promedio / i
}

v_tiempopromedio

# ordenamos los datos en una tabla
final <- data.frame(
    "No. Pieza" = v_nopieza,
    "Pieza" = r1,
    "Tiempo entre llegadas" = v_uniforme,
    "Tiempo de llegada" = v_tiempollegada,
    "Inicio de la inspección" = v_inicioinspeccion,
    "Operario" = v_operario,
    "Tiempo de inspección" = v_distnorm,
    "Fin de la inspección" = v_fininspeccion,
    "Tiempo en inspección" = v_tiempoeninspeccion,
    "Tiempo en espera" = v_tiempoespera,
    "Tiempo promedio de inspección" = v_tiempopromedio
)
final
