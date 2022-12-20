dist_triangular <- function(i, min, moda, max) {
    comparacion <- (moda - min) / (max - min)
    r <- data.frame(r1, r2)
    r <- data.frame(r,
        "Longitud barra 1" = ifelse(r$r1 <= comparacion,
            min + ((moda - min) * sqrt(r$r2)),
            max - (max - moda) * sqrt(1 - r$r2)
        )
    )
    return(r)
}

distribucion_erlang <- function(i, k, m) {
    for (i in i) {
        pt <- -((m / k) * log(r3 * r4 * r5))
    }
    v_erlang <- pt
    return(v_erlang)
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


v_triangular <- numeric()
v_erlang <- numeric()
v_longitudtotal <- numeric()
v_estado <- numeric()
v_promedio <- numeric()


# Pieza 1
res_triangular <- dist_triangular(length(r1), 45, 50, 55)
res_triangular


#Pieza 2
res_erlang <- distribucion_erlang(length(r1), 3, 30)
res_erlang
#Longitud total
v_longitudtotal <- res_erlang + res_triangular$Longitud.barra.1
v_longitudtotal

#Estado de la barra
v_estado <- ifelse(
    v_longitudtotal >= 70 & v_longitudtotal <= 90,
    1, 0
)
v_estado




final <- data.frame(
    res_triangular,
    "Longitud barra 2" = res_erlang,
    "Longitud total" = v_longitudtotal,
    "Estado de la barra" = ifelse(
        v_longitudtotal >= 70 & v_longitudtotal <= 90,
        "Aceptado", "Rechazado"
    )
)

final
