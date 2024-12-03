# seguimos agora com o treino 2 para a prova da DEISY


# função de transporte


transporte <- function(meio){
  if(meio == "carro"){
    cat("GASOLINA / DIESEL")
  }
  if(meio == "moto"){
    cat("GASOLINA")
  }
  if(meio == "bicicleta"){
    cat("HUMANA (sem combustível")
  }
  if(meio == "onibus"){
    cat("DIESEL")
  }
  if(meio == "trem"){
    cat("ELETRECIDADE / DIESEL")
  }
  if(meio == "aviao"){
    cat("QUEROSENE")
  }
  if(meio == "barco"){
    cat("DIESEL / GASOLINA")
  }
}

x <- "onibus"

transporte(x)



# ------------------------------------------------

temperatura <- function(x, unidade){
  if(unidade == "celcius"){
    final = (x*(9/5)) + 32
  }
  if(unidade == "fahrenheit"){
    final = (x - 32) / (9/5)
  }
  return(final)
}

unidade <- "fahrenheit"
x <- 30

bah <- temperatura(x, unidade)


# --------------------------------------------------


peso <- 76
altura <- 177

calculadora_imc <- function(peso, altura){
  
  imc = peso/((altura/100)^2)
  if(imc < 18.5){
    cat("parceiro, vc é magro pra caralho")
  }
  if(imc >= 18.5 && imc <= 24.9){
    cat("parceiro, vc é normal, parabens!")
  }
  if(imc > 24.9 && imc <= 29.9){
    cat("parceiro, vc é cheinho")
  }
  if(imc > 29.9 && imc <= 34.9){
    cat("parceiro, vc só é gordo")
  }
  if(imc > 34.9 && imc <= 39.9){
    cat("parceiro, vc é gordo pra caralho")
  }
  if(imc > 39.9){
    cat("parceiro, vc é ENORME")
  }
}


imc_vitor <- calculadora_imc(peso, altura)
imc_luana <- calculadora_imc(peso, altura)




