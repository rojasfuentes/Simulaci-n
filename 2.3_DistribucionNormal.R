dist_normal_ej <- function(i, media, des_es) {
      r1 <- c(
            0.3501, 0.5185, 0.9342, 0.7302, 0.0396, 0.5146, 0.1977,
            0.0382, 0.8621, 0.1821, 0.1196, 0.544
      )

      for (i in i) {
            res <- (sum(r1) - (media / 2)) * des_es + media
      }
      t1 <- t(r1)
      final <- data.frame(t1)
      final <- cbind(final, res)
      return(final)
}

dist_normal_ej(1:6, 12, 0.4)


random_rumbers <- function(ncol, nrow) {
      dt <- data.frame(matrix(ncol = ncol, nrow = nrow))
      for (nrow in 1:nrow) {
            for (ncol in 1:ncol) {
                  dt[nrow, ncol] <- runif(1, 0, 1)
            }
      }
      return(dt)
}

dist_normal <- function(i, media, des_es) {
      r <- random_rumbers(media, i)
      for (i in 1:i) {
            r$resultado[i] <- (sum(r[i, ]) - (media / 2)) * des_es + media
      }
      return(r)
}

iteraciones <- as.numeric(readline("Ingrese el número de iteraciones: "))
media <- as.numeric(readline("Ingrese la media: "))
des_es <- as.numeric(readline("Ingrese la desviación estándar: "))

dist_normal(iteraciones, media, des_es)
