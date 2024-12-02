

#fazendo tudo de novo para o treino da prova da deisy



require(data.table)

dados <- fread("C:\\Users\\FUJIMOTO\\Documents\\treino_deisy\\pokemon.csv")

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
      # é pra ter um IF aqui das outras restrições  
        
        
    
    }
  }
  return(matriz) 
}

matriz_resultado <- varredura_matriz(matriz_a)
# -------------------------------------------------


matriz_quadrado_perfeito <- varredura_matriz_quadrado(matriz_a)

# ---------------------------------------------






