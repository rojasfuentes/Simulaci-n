r <- c(
    3.1528, 3.2077, 2.0432, 5.1535, 3.9651,
    2.7069, 0.6329, 1.1016, 1.9163, 5.8579,
    15.6413, 6.5981, 9.3736, 14.5247, 1.5698,
    0.9931, 7.1830, 0.9498, 3.9832, 4.4335,
    5.3960, 0.4288, 5.3946, 0.3550, 13.0162,
    0.7018, 3.1421, 15.4646, 2.2953, 5.6451,
    1.0767, 0.9668, 5.0190, 9.4375, 1.7520,
    14.9830, 4.1234, 9.6052, 0.9688, 25.7276,
    0.8848, 1.2833, 2.2067, 14.1017, 1.1787,
    7.0210, 22.7779, 10.2609, 9.1922, 5.4476
)
# numero de datos
n <- length(r)
# intervalos
k <- length(repetidos)
# calculamos la media muestral
media <- mean(r)

# calculamos la varianza muestral
varianza <- var(r)

# parametros
lambda <- media
# les quitamos los decimales
r2 <- round(r, 0)
# cuantas veces se repite cada numero
repeticiones <- table(r2)
# mostrar en un histograma
hist(r2, breaks = 50, freq = FALSE, col = "blue", main = "Histograma de la variable aleatoria", xlab = "Valores de la variable aleatoria", ylab = "Frecuencia")

m0_1 <- sum(repeticiones[1:2])
m2_3 <- sum(repeticiones[3:4])
m4_5 <- sum(repeticiones[5:6])
m6_7 <- sum(repeticiones[7:8])
m8_9 <- sum(repeticiones[9])
m10_11 <- sum(repeticiones[10])
m12_13 <- sum(repeticiones[11])
m14_15 <- sum(repeticiones[12:13])
m16_17 <- sum(repeticiones[14])
m18_19 <- 0
m20_21 <- 0
m22_23 <- sum(repeticiones[15])
m24_25 <- 0
m26_27 <- sum(repeticiones[16])

# Agrupamos los numeros en un vector
repetidos <- c(
    m0_1, m2_3, m4_5, m6_7, m8_9,
    m10_11, m12_13, m14_15, m16_17,
    m18_19, m20_21, m22_23, m24_25,
    m26_27
)
repetidos
# Probabilidad obvservada en cada intercvalo
poi <- repetidos / sum(repetidos)
poi

# Probabilidad acumulada
acum <- cumsum(poi)

# Probabilidad acumulada de weibull
acum_weibull <- 1 - exp(-lambda * (repetidos - 0.5))

