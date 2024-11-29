


#treino deisy


x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
media_x <- mean(x)
mediana_x <- median(x)

s_quadrado <- function(x, media){
  sum((x - media)^2)/length(x)
}

dma <- function(x, media){
  sum(abs(x - media)^2)/length(x) - 1
}


as2 <- function(x, media, mediana){
  3*(media - mediana)/sd(x)
}

bah <- dma(x, media_x)
s2 <- s_quadrado(x, media_x)
eita <- as2(x, media_x, mediana_x)
