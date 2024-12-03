

#fazendo tudo de novo para o treino da prova da deisy


install.packages("data.table")
install.packages("dplyr")

require(data.table)
require(dplyr)

dados <- fread("C:\\Users\\vitor\\OneDrive\\Documentos\\treino_deisy\\pokemon.csv")

s_quadrado <- function(x, media){
  sum((x-media)^2)/length(x)
}


vetor_attack <- c(dados$Attack)
media_vetor_attack <- mean(vetor_attack)
variancia_pokemon <- s_quadrado(vetor_attack, media_vetor_attack) 
#sla se essa variancia esta certa, mas ok... resultado: 1004,958

vetor <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
media_vetor <- mean(vetor)
variancia_vetor <- s_quadrado(vetor, media_vetor)

#estudo de matrizes - varredura de uma matriz

matriz_a <- matrix(c(1:9),
            ncol = 3, 
            byrow = TRUE)

# ----------------------------------------------
eh_primo <- function(n){
  if(n <= 1){
    return(FALSE)
  }
  if(n == 2){
    return(TRUE)
  }
  for(i in 2:sqrt(n)){
    if(n %% i == 0){
      return(FALSE)
    }
  }
  return(TRUE)
}

quadrado_perfeito <- function(n){
  if (n < 0) {
    return(FALSE) 
  }
  return(sqrt(n) == floor(sqrt(n)))
}



varredura_matriz <- function(matriz){
  for(i in 1:nrow(matriz)){
    for(j in 1:ncol(matriz)){
      if(eh_primo(matriz[i, j]) == TRUE){
        matriz[i, j] = matriz[i, j] * 8
      }
      if(quadrado_perfeito(matriz[i, j]) == TRUE){
        matriz[i, j] = matriz[i, j] - 19
        if(matriz[i, j] < 0){
          matriz[i, j] = matriz[i, j]^7
        }
    
      }
      if(matriz[i, j] < 0){
        matriz[i, j] = (abs(matriz[i, j]))^(1/3)
      }
      #aqui acabam as restrições, questao SOLADA!
    }
  }
  return(matriz) 
}

matriz_resultado <- varredura_matriz(matriz_a)
# -------------------------------------------------

#criação de matriz


matriz_exercicio_1 <- matrix(c(3, -3, 6, -8,
                             1, 2, -10, -7,
                             8, -2, -9, -4,
                             10, -5, -1, 7),
                           ncol = 4,
                           byrow = TRUE)

matriz_exercicio_2 <- matrix(c(-1, 8, 13, -16,
                               0, 16, 18, -6,
                               -18, -15, -2, -11,
                               11, -12, -13, 3,
                               -4, 5, -19, 17),
                             ncol = 4,
                             byrow = TRUE)


matriz_exercicio_3 <- matrix(c(-22, 14, 21, 19,
                               23, -20, -17, 17,
                               -7, -11, -23, 28,
                               24, -14, -5, 8),
                             ncol = 4,
                             byrow = TRUE)


exercicio_3 <- varredura_matriz(matriz_exercicio_3)

  

# -------------------------------------------------
  
#fazendo agora o exercicio 3 da lista de treino


#preciso do banco de dados para fazer essa questao


# EXERCICIOS DO CHATGPT

cv <- function(x){
  coeficiente_variacao = round((sd(x)/mean(x))*100, 2)
  return(coeficiente_variacao)
}

dados <- airquality
dados_limpos <- na.omit(airquality)

sum(is.na(dados_limpos))

cv(airquality$Wind)
cv(dados_limpos$Ozone)
cv(airquality$Temp)
cv(dados_limpos$Solar.R)













