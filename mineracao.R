source("exploracao.R")

######### Configurações Gerais ######### 

nome_img = "imagem"
qtd = 3
precisao_necessaria = 0.8

######################################### 


tabela = gerais
resposta = matrix(data = NA, nrow = 2, ncol = qtd)

cont = 1
while (cont <= qtd) {
  # Treinamento e Plot
  aux = sample.int(nrow(tabela))
  
  aux = aux[1:floor(0.7 * length(aux))]
  
  tabela[sample.int(nrow(tabela)), ]
  
  
  treinamento = tabela [aux, ]
  
  validacao = tabela [-aux, ]
  
  
  # Resultado
  resultado = rpart(TEM_SEG_IMOBILIARIO ~ ., data = treinamento)
  
  # Observando Métricas
  predicao = predict(resultado, newdata = validacao[, -1], type = "class")
  matriz_confusao = table(predicao, validacao$TEM_SEG_IMOBILIARIO)
  matriz_confusao
  acuracia = sum(diag(matriz_confusao)) / sum(matriz_confusao)
  acuracia
  
  precisao_vida = diag(matriz_confusao)[2] / sum(matriz_confusao[2, ])
  precisao_vida
  
  if (precisao_vida > precisao_necessaria) {
    #guarda acuracia e precisao
    resposta[, cont] <- c(acuracia, as.numeric(precisao_vida))
    ## Criando Imagem
    gerarpdf = paste('imagens/', nome_img, cont, '.pdf', sep = '')
    pdf(gerarpdf, paper = "a4r")
    fancyRpartPlot(resultado)
    dev.off()
    cont = cont + 1
  }
}