r1 <- c(
    0.5679, 0.1617, 0.4787, 0.6557, 0.1373,
    0.3688, 0.1512, 0.4434, 0.5308, 0.8793
)
r2 <- c(
    0.1242, 0.9168, 0.3928, 0.3727, 0.7997,
    0.0014, 0.1197, 0.8267, 0.6528, 0.6544
)

# Ejemplo r3 <- c( 0.7301, 0.3046, 0.2781, 0.7339, 0.8609, 0.1148 )

v_uniforme <- numeric()
v_erlang <- numeric()
v_longitudtotal <- numeric()
v_estado <- numeric()
v_promedio <- numeric()
dist_uniforme <- function(i, ri, a, b) {
    for (i in 1:i) {
        x1 <- a + (b - a) * ri
        v_uniforme <- x1
    }
    return(v_uniforme)
}

distribucion_erlang <- function(i, k, m) {
    for (i in i) {
        pt <- -((m / k) * log(r1 * r2))
    }
    v_erlang <- pt
    # final <- data.frame(r1, r2, "Tiempo Proceso min/pza" = pt)
    return(v_erlang)
}

res_erlang <- distribucion_erlang(length(r1), 4, 30)
res_uniforme <- dist_uniforme(length(r1), r1, 45, 55)
v_longitudtotal <- res_erlang + res_uniforme
v_estado <- ifelse(
    v_longitudtotal >= 70 & v_longitudtotal <= 90,
    1, 0
)
v_estado


suma <- 0
for (i in seq_along(v_estado)) {
    suma <- suma + v_estado[i]
    v_promedio[i] <- suma / i
}
v_promedio

final <- data.frame(
    "r1" = r1,
    "r2" = r2,
    "Longitud barra 1" = res_uniforme,
    "Longitud barra 2" = res_erlang,
    "Longitud total" = v_longitudtotal,
    "Estado de la barra" = ifelse(
        v_longitudtotal >= 70 & v_longitudtotal <= 90,
        "Aceptado", "Rechazado"
    ),
    "Prob Fuera De Las Especificaciones" = v_promedio * 100
)

final
