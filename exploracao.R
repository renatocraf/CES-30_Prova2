source("carregamento_dados.R")

# Trabalho observando os leveis de cada fator (numero maximo de leveis : 5)

# Trabalhando com pessoas
{
  ##################### observando ESTADOS #######################
  {
    estado <- pessoas %>% group_by(ESTADO) %>% count()
    # Quantidade de estados
    length(estado$n)
    # Contagem de pessoas por estado
    estado
    # Plotando Qtd de Pessoas x Estado
    plot(estado)
    # Ordenando
    estado[order(estado$n, decreasing = TRUE), ]
    # Vamos dividir o estado em 4 grupos: NY, CA, MI e "Outros"
    
    #removendo o array estados
    remove(estado)
  }
  
  ##################### observando PROFISSAO #######################
  {
    profissao <- pessoas %>% group_by(PROFISSAO) %>% count()
    # Quantidade de estados
    length(profissao$n)
    # Contagem de pessoas por estado
    profissao
    # Plotando Qtd de Pessoas x Estado
    plot(profissao)
    # Ordenando
    profissao[order(profissao$n, decreasing = TRUE), ]
    #Vamos as Profissoes em 4 grupos: "Programmer/Developer","IT Staff","Nurse" e "Outros"
    
    #removendo o array profissoes
    remove(profissao)
    
  }
  
  ##################### observando IDADE #######################
  {
    idade <- pessoas %>% group_by(IDADE) %>% count()
    # Observando os valores
    idade
    # Plotando para ter uma melhor noção
    plot(idade)
    hist(as.integer( as.character( pessoas$IDADE)))
    # Verificando maior idade
    valor = as.character(idade$IDADE)
    max(valor)
    # Vamos dividir a idade em 5 grupos: "0-Não Informado",
    #"17-35","35-50","50-65","+65"
    
    #removendo o array estados
    remove(idade)
    
  }
  
  ##################### observando N_DEPENDENTES #######################
  {
    dependentes <- pessoas %>% group_by(N_DEPENDENTES) %>% count()
    # Observando os valores
    dependentes
    # Plotando para ter uma melhor noção
    plot(dependentes)
    # Verificando maior idade
    valor[7]
    # Vamos dividir OS dependentes em 4 grupos: "0","1-2","3","+4"
    
    #removendo o array estados
    remove(dependentes)
  }
  
  
  # Fazendo filtro para a tabela pessoas
  {
    pessoas2 = pessoas
    
    ## N_DEPENDENTES
    {
      # tem q transformar em CHAR antes de mudar para int
      pessoas2$N_DEPENDENTES = as.character(pessoas2$N_DEPENDENTES)
      pessoas2$N_DEPENDENTES = as.integer(pessoas2$N_DEPENDENTES)
      
      # criando nova coluna
      pessoas2$DEPENDENTES[pessoas2$N_DEPENDENTES == 0] = "0"
      pessoas2$DEPENDENTES[pessoas2$N_DEPENDENTES >= 1 &
                             pessoas2$N_DEPENDENTES < 3] = "1-2"
      pessoas2$DEPENDENTES[pessoas2$N_DEPENDENTES >= 3 &
                             pessoas2$N_DEPENDENTES < 50] = "3"
      pessoas2$DEPENDENTES[pessoas2$N_DEPENDENTES >= 4] = "+4"
      
      #voltando para factor
      pessoas2$DEPENDENTES = as.factor(pessoas2$DEPENDENTES)
      
      #removendo coluna
      pessoas2 <- pessoas2[, -7]
    }
    
    ## IDADE
    {
      # tem q transformar em CHAR antes de mudar para int
      pessoas2$IDADE = as.character(pessoas2$IDADE)
      pessoas2$IDADE = as.integer(pessoas2$IDADE)
      
      # criando nova coluna
      pessoas2$FAIXA_ETARIA[pessoas2$IDADE == 0] = "Não Informado"
      pessoas2$FAIXA_ETARIA[pessoas2$IDADE >= 17 &
                              pessoas2$IDADE < 35] = "17-35"
      pessoas2$FAIXA_ETARIA[pessoas2$IDADE >= 35 &
                              pessoas2$IDADE < 50] = "35-50"
      pessoas2$FAIXA_ETARIA[pessoas2$IDADE >= 50 &
                              pessoas2$IDADE < 65] = "50-65"
      pessoas2$FAIXA_ETARIA[pessoas2$IDADE >= 65] = "+65"
      
      #voltando para factor
      pessoas2$FAIXA_ETARIA = as.factor(pessoas2$FAIXA_ETARIA)
      
      #removendo coluna
      pessoas2 <- pessoas2[, -5]
    }
    
    ## PROFISSAO
    {
      # tem q transformar em CHAR antes de mudar para int
      pessoas2$PROFISSAO = as.character(pessoas2$PROFISSAO)
      
      # criando nova coluna
      pessoas2$PROFISSOES[pessoas2$PROFISSAO != ""] = "Outros"
      pessoas2$PROFISSOES[pessoas2$PROFISSAO == "Programmer/Developer"] =
        "Programmer/Developer"
      pessoas2$PROFISSOES[pessoas2$PROFISSAO == "IT Staff"] = "IT Staff"
      pessoas2$PROFISSOES[pessoas2$PROFISSAO == "Nurse"] = "Nurse"
      
      
      #voltando para factor
      pessoas2$PROFISSOES = as.factor(pessoas2$PROFISSOES)
      
      #removendo coluna
      pessoas2 <- pessoas2[, -4]
    }
    
    ## ESTADOS
    {
      # tem q transformar em CHAR antes de mudar para int
      pessoas2$ESTADO = as.character(pessoas2$ESTADO)
      
      # criando nova coluna
      pessoas2$ESTADOS[pessoas2$ESTADO != ""] = "Outros"
      pessoas2$ESTADOS[pessoas2$ESTADO == "NY"] = "NY"
      pessoas2$ESTADOS[pessoas2$ESTADO == "CA"] = "CA"
      pessoas2$ESTADOS[pessoas2$ESTADO == "MI"] = "MI"
      
      #voltando para factor
      pessoas2$ESTADOS = as.factor(pessoas2$ESTADOS)
      
      #removendo coluna
      pessoas2 <- pessoas2[, -1]
    }
  }
  
}

# Trabalhando com financeiro
{
  ##################### observando SALARIO #######################
  {
    salario <- financeiro %>% group_by(SALARIO) %>% count()
    # Verificando maior idade
    valor = as.character(salario$SALARIO)
    # valor minimo
    valor[1]
    # valor maximo
    valor[998]
    # construindo histograma para ter noção da divisão
    todos_salarios = as.character(financeiro$SALARIO)
    hist(as.integer(todos_salarios))
    
    # Vamos dividir OS dependentes em 3 grupos: "-60k","60-80k","+80"
    
    #removendo o array
    remove(salario)
    remove(todos_salarios)
    remove(valor)
  }
  
  ##################### observando CC_SALDO_MEDIO #######################
  {
    saldo <- financeiro %>% group_by(CC_SALDO_MEDIO) %>% count()
    saldo
    # Verificando maior idade
    valor = as.character(saldo$CC_SALDO_MEDIO)
    # valor minimo
    valor[1]
    # valor maximo
    valor[93]
    # construindo histograma para ter noção da divisão
    todos_saldos = as.character(financeiro$CC_SALDO_MEDIO)
    hist(as.integer(todos_saldos))
    
    # Vamos dividir OS dependentes em 3 grupos: "0","1-50k","+50k"
    
    #removendo o array
    remove(saldo)
    remove(todos_saldos)
    remove(valor)
  }
  
  ##################### observando VALOR_FUNDOS_BANCARIOS #######################
  {
    fundo <- financeiro %>% group_by(VALOR_FUNDOS_BANCARIOS) %>% count()
    fundo
    # Verificando maior idade
    valor = as.character(fundo$VALOR_FUNDOS_BANCARIOS)
    # valor minimo
    valor[1]
    # valor maximo
    valor[280]
    # construindo histograma para ter noção da divisão
    todos_fundos = as.character(financeiro$VALOR_FUNDOS_BANCARIOS)
    hist(as.integer(todos_fundos))
    
    # Vamos dividir OS dependentes em 3 grupos: "0","1-10k","+10k"
    
    #removendo o array
    remove(fundo)
    remove(todos_fundos)
    remove(valor)
  }
  
  ##################### observando VALOR_FIN_IMOBILIARIO #######################
  {
    fin_imob <- financeiro %>% group_by(VALOR_FIN_IMOBILIARIO) %>% count()
    fin_imob
    # Verificando maior idade
    valor = as.character(fin_imob$VALOR_FIN_IMOBILIARIO)
    # valor minimo
    valor[1]
    # valor maximo
    valor[242]
    # construindo histograma para ter noção da divisão
    todos_fin_imob = as.character(financeiro$VALOR_FIN_IMOBILIARIO)
    hist(as.integer(todos_fin_imob))
    
    # Vamos dividir OS dependentes em 3 grupos: "0","1-10k","+10k"
    
    #removendo o array
    remove(fin_imob)
    remove(todos_fin_imob)
    remove(valor)
  }
  
  ##################### observando CREDIT_CARD_LIMITE #######################
  {
    credito <- financeiro %>% group_by(CREDIT_CARD_LIMITE) %>% count()
    credito
    # Verificando maior idade
    valor = as.character(credito$CREDIT_CARD_LIMITE)
    # valor minimo
    valor[1]
    # valor maximo
    valor[23]
    # construindo histograma para ter noção da divisão
    todos_credito = as.character(financeiro$CREDIT_CARD_LIMITE)
    hist(as.integer(todos_credito))
    
    # Vamos dividir OS dependentes em 2 grupos: "500-1k","+1k"
    
    #removendo o array
    remove(credito)
    remove(todos_credito)
    remove(valor)
  }
  
  # Fazendo filtro para a tabela financeiro
  {
    financeiro2 = financeiro
    
    ## CREDIT_CARD_LIMITE
    {
      # tem q transformar em CHAR antes de mudar para int
      financeiro2$CREDIT_CARD_LIMITE = as.character(financeiro2$CREDIT_CARD_LIMITE)
      financeiro2$CREDIT_CARD_LIMITE = as.integer(financeiro2$CREDIT_CARD_LIMITE)
      
      # criando nova coluna
      financeiro2$CREDITO_LIMIT[financeiro2$CREDIT_CARD_LIMITE >= 500 &
                                  financeiro2$CREDIT_CARD_LIMITE <= 1000] = "500-1K"
      financeiro2$CREDITO_LIMIT[financeiro2$CREDIT_CARD_LIMITE > 1000] =
        "+1K"
      
      #voltando para factor
      financeiro2$CREDITO_LIMIT = as.factor(financeiro2$CREDITO_LIMIT)
      
      #removendo coluna
      financeiro2 <- financeiro2[, -6]
    }
    
    ## VALOR_FIN_IMOBILIARIO
    {
      # tem q transformar em CHAR antes de mudar para int
      financeiro2$VALOR_FIN_IMOBILIARIO = as.character(financeiro2$VALOR_FIN_IMOBILIARIO)
      financeiro2$VALOR_FIN_IMOBILIARIO = as.integer(financeiro2$VALOR_FIN_IMOBILIARIO)
      
      # criando nova coluna
      financeiro2$FIN_IMOB[financeiro2$VALOR_FIN_IMOBILIARIO == 0] = "0"
      financeiro2$FIN_IMOB[financeiro2$VALOR_FIN_IMOBILIARIO > 0 &
                             financeiro2$VALOR_FIN_IMOBILIARIO <= 10000] = "1-10K"
      financeiro2$FIN_IMOB[financeiro2$VALOR_FIN_IMOBILIARIO > 10000] =
        "+10K"
      
      #voltando para factor
      financeiro2$FIN_IMOB = as.factor(financeiro2$FIN_IMOB)
      
      #removendo coluna
      financeiro2 <- financeiro2[, -4]
    }
    
    ## VALOR_FUNDOS_BANCARIOS
    {
      # tem q transformar em CHAR antes de mudar para int
      financeiro2$VALOR_FUNDOS_BANCARIOS = as.character(financeiro2$VALOR_FUNDOS_BANCARIOS)
      financeiro2$VALOR_FUNDOS_BANCARIOS = as.integer(financeiro2$VALOR_FUNDOS_BANCARIOS)
      
      # criando nova coluna
      financeiro2$FUNDOS_BANC[financeiro2$VALOR_FUNDOS_BANCARIOS == 0] =
        "0"
      financeiro2$FUNDOS_BANC[financeiro2$VALOR_FUNDOS_BANCARIOS > 0 &
                                financeiro2$VALOR_FUNDOS_BANCARIOS <= 10000] = "1-10K"
      financeiro2$FUNDOS_BANC[financeiro2$VALOR_FUNDOS_BANCARIOS > 10000] =
        "+10K"
      
      #voltando para factor
      financeiro2$FUNDOS_BANC = as.factor(financeiro2$FUNDOS_BANC)
      
      #removendo coluna
      financeiro2 <- financeiro2[, -3]
    }
    
    ## CC_SALDO_MEDIO
    {
      # tem q transformar em CHAR antes de mudar para int
      financeiro2$CC_SALDO_MEDIO = as.character(financeiro2$CC_SALDO_MEDIO)
      financeiro2$CC_SALDO_MEDIO = as.integer(financeiro2$CC_SALDO_MEDIO)
      
      # criando nova coluna
      financeiro2$SALDO_MEDIO[financeiro2$CC_SALDO_MEDIO == 0] = "0"
      financeiro2$SALDO_MEDIO[financeiro2$CC_SALDO_MEDIO > 0 &
                                financeiro2$CC_SALDO_MEDIO <= 50000] = "1-50K"
      financeiro2$SALDO_MEDIO[financeiro2$CC_SALDO_MEDIO > 50000] = "+50K"
      
      #voltando para factor
      financeiro2$SALDO_MEDIO = as.factor(financeiro2$SALDO_MEDIO)
      
      #removendo coluna
      financeiro2 <- financeiro2[, -2]
    }
    
    ## SALARIO
    {
      # tem q transformar em CHAR antes de mudar para int
      financeiro2$SALARIO = as.character(financeiro2$SALARIO)
      financeiro2$SALARIO = as.integer(financeiro2$SALARIO)
      
      # criando nova coluna
      financeiro2$SALARIO_ANUAL[financeiro2$SALARIO <= 60000] = "-60K"
      financeiro2$SALARIO_ANUAL[financeiro2$SALARIO > 60000 &
                                  financeiro2$SALARIO < 80000] = "60-80K"
      financeiro2$SALARIO_ANUAL[financeiro2$SALARIO >= 80000] = "+80K"
      
      #voltando para factor
      financeiro2$SALARIO_ANUAL = as.factor(financeiro2$SALARIO_ANUAL)
      
      #removendo coluna
      financeiro2 <- financeiro2[, -1]
    }
    
  }
  
}

# Trabalhando seguro
{
  ##################### observando TEMPO_CLIENTE_ANOS #######################
  {
    tempo <- seguro %>% group_by(TEMPO_CLIENTE_ANOS) %>% count()
    tempo
    # Verificando maior idade
    valor = as.character(tempo$TEMPO_CLIENTE_ANOS)
    # valor minimo
    valor[1]
    # valor maximo
    valor[5]
    # construindo histograma para ter noção da divisão
    todos_tempo = as.character(seguro$TEMPO_CLIENTE_ANOS)
    hist(as.integer(todos_tempo))
    
    # Vamos dividir OS dependentes em 2 grupos: "-2anos","+2anos"
    
    #removendo o array
    remove(tempo)
    remove(todos_tempo)
    remove(valor)
  }
  
  # Fazendo filtro para a tabela seguro
  seguro2 = seguro
  
  ## TEMPO_CLIENTE_ANOS
  {
    # tem q transformar em CHAR antes de mudar para int
    seguro2$TEMPO_CLIENTE_ANOS = as.character(seguro2$TEMPO_CLIENTE_ANOS)
    seguro2$TEMPO_CLIENTE_ANOS = as.integer(seguro2$TEMPO_CLIENTE_ANOS)
    
    # criando nova coluna
    seguro2$TEMPO[seguro2$TEMPO_CLIENTE_ANOS <= 2] = "1 ou 2 anos"
    seguro2$TEMPO[seguro2$TEMPO_CLIENTE_ANOS > 2] = "+2anos"
    
    #voltando para factor
    seguro2$TEMPO = as.factor(seguro2$TEMPO)
    
    #removendo coluna
    seguro2 <- seguro2[, -2]
  }
  
}

#remove tabelas "1"
remove(pessoas)
remove(financeiro)
remove(seguro)

# Unindo tabelas
gerais = cbind(seguro2,pessoas2,financeiro2,ltv)

#remove tabelas "2"
remove(pessoas2,ltv,seguro2,financeiro2)

# Sobra agora apenas o data.frame gerais






