distribucion_erlang <- function(i, k, m) {
    r1 <- r1
    r2 <- r2

    for (i in i) {
        v_tiempollegada[i] <- -((m / k) * log(r1[i] * r2[i]))
    }
    return(v_tiempollegada)
}
dist_normal <- function(r1, r2, media, desviacion) {
    for (i in seq_along(r1)) {
        v_inicioatencion[i] <- media + desviacion * sqrt(-2 * log(r1[i])) * cos(2 * pi * r2[i])
    }
    return(v_inicioatencion)
}
dist_uniforme <- function(r, a, b) {
    for (i in seq_along(r)) {
        v_litros90[i] <- a + (b - a) * r[i]
    }
    return(v_litros90)
}
dist_uniforme97 <- function(r, a, b) {
    for (i in seq_along(r)) {
        v_litros97[i] <- a + (b - a) * r[i]
    }
    return(v_litros97)
}
dist_exponencial <- function(r, media) {
    for (i in seq_along(r)) {
        v_tiempollegada[i] <- -media * (log(1 - r1[i]))
    }
    return(v_tiempollegada)
}
# Variables aleatorias
r1 <- c(
    0.9957, 0.0834, 0.3559, 0.9300, 0.3344,
    0.2159, 0.5100, 0.4083, 0.7563, 0.5663
)

r2 <- c(
    0.3607, 0.1337, 0.2347, 0.0569, 0.0577,
    0.8212, 0.6834, 0.0166, 0.4735, 0.0811
)
r3 <- c(
    0.1115, 0.8269, 0.1018, 0.5603, 0.1441,
    0.1393, 0.7107, 0.2386, 0.0343, 0.7761
)
r4 <- c(
    0.1752, 0.9609, 0.2321, 0.6144, 0.9280,
    0.6714, 0.0319, 0.3006, 0.8876, 0.8315
)
r5 <- c(
    0.6819, 0.5947, 0.3621, 0.0667, 0.0174,
    0.6092, 0.1016, 0.0768, 0.0942, 0.3927
)
r6 <- c(
    0.5791, 0.1264, 0.8955, 0.6294, 0.3102,
    0.8488, 0.4119, 0.1195, 0.0884, 0.3851
)
r7 <- c(
    0.7904, 0.1954, 0.4184, 0.2454, 0.1668,
    0.6191, 0.6714, 0.9081, 0.0400, 0.3709
)
r8 <- c(
    0.7623, 0.1798, 0.1820, 0.7297, 0.7962,
    0.0571, 0.4589, 0.9184, 0.6230, 0.8279
)

# Vectores vacios
v_tiempollegada <- numeric()
v_inicioatencion <- numeric()
v_tipogasolina <- c()
v_litros90 <- numeric()
v_litros95 <- numeric()
v_litros97 <- numeric()

# r1 #Tiempo de llegada  --- Erland, k:2- erlang | media: 7
# r2 #Erlang
# r3 #Inicio de atencion --- Normal | media: 3.5 | desviacion estandar: 1
# r4  #Normal
# r5 #Tipo de gasolina --- 90 octanos : 25% | 95 octanos :30% | 98 octanos : 45%
# r6 # 95 oct Exponencial | media: 1.8
# r7 # 97 oct uniforme | min: 12 | max: 28
# r8 #90 oct uniforme | min: 22 | max: 40


# Tiempo de llegada
v_tiempollegada <- distribucion_erlang(1:10, 2, 7)
v_tiempollegada

# Hora de llegada
v_horallegada <- cumsum(v_tiempollegada)
v_horallegada

# Inicio de atencion
v_inicioatencion <- dist_normal(r3, r4, 3.5, 1)

# Tipo de gasolina
for (i in seq_along(r5)) {
    if (r5[i] <= 0.25) {
        v_tipogasolina[i] <- "90 octanos"
    } else if (r5[i] <= 0.55) {
        v_tipogasolina[i] <- "95 octanos"
    } else {
        v_tipogasolina[i] <- "97 octanos"
    }
}

# calculamos la gasolina para 90, 95 y 97 octanos
v_litros90 <- dist_uniforme(r8, 22, 40)
v_litros95 <- dist_exponencial(r6, 1.8)
v_litros97 <- dist_uniforme97(r7, 12, 28)


# Ingresamos los litros de gasolina
v_litros <- ifelse(v_tipogasolina == "90 octanos", v_litros90,
    ifelse(v_tipogasolina == "95 octanos", v_litros95, v_litros97)
)

# Limitamos las tablas a NA
v_litros90 <- ifelse(v_tipogasolina == "90 octanos", v_litros90, NA)
v_litros95 <- ifelse(v_tipogasolina == "95 octanos", v_litros95, NA)
v_litros97 <- ifelse(v_tipogasolina == "97 octanos", v_litros97, NA)

# creamos el data frame

df <- data.frame(
    "Hora de llegada" = v_tiempollegada,
    "Tiempo de llegada" = v_horallegada,
    "Inicio de atencion" = v_inicioatencion,
    "Tiempo de atencion" = cumsum(v_inicioatencion),
    "Tipo de gasolina" = v_tipogasolina,
    "Cantidad de litros" = v_litros,
    v_litros90,
    v_litros95,
    v_litros97
)

df
