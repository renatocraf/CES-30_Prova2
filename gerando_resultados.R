#source("mineracao.R")

# previsao <- function(tabela,qtd,nome_img){
#   resposta = matrix(data=NA,nrow=2,ncol = qtd)
#   for (i in 1:qtd){
#     # Treinamento e Plot
#     aux = sample.int(nrow(tabela));
#     aux = aux[1:floor(0.7*length(aux))];
#     tabela[sample.int(nrow(tabela)),];
#     
#     treinamento = tabela [aux,];
#     validacao = tabela [-aux,];
#     
#     # Resultado
#     resultado = rpart(TEM_SEG_IMOBILIARIO ~ ., data=treinamento)
#     
#     ## Criando Imagem
#     gerarpdf = paste('imagens/',nome_img,i,'.pdf',sep='')
#     pdf(gerarpdf, paper = "a4r")
#     fancyRpartPlot(resultado)
#     dev.off()
#     
#     # Observando Métricas
#     predicao = predict(resultado, newdata = validacao[,-1], type = "class")
#     matriz_confusao = table(predicao, validacao$TEM_SEG_IMOBILIARIO)
#     matriz_confusao
#     acuracia = sum(diag(matriz_confusao))/sum(matriz_confusao)
#     acuracia
#     
#     precisao_vida = diag(matriz_confusao)[2]/sum(matriz_confusao[2,])
#     precisao_vida
#     
#     resposta[,i] <- c(acuracia,as.numeric(precisao_vida) )
#   }
#   return(resposta)
# }
# 
# resposta =previsao(gerais,10,"teste")
# 
# write.table(x = resposta, file = "imagens/acuracia_e_precisaovida.csv", sep = ';')
