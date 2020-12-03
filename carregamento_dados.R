# verificando e instalando bibliotecas necessarias
if (!require("rattle")) {
  source('instalando_bibli.R')
}

# Importando Bibliotecas
library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)

# Extraindo dados do CSV
dados_pessoais = read.csv('dados/cliente_dados_pess.csv',
                          header = TRUE,
                          sep = ';')

dados_financeiro = read.csv('dados/cliente_financeiro.csv',
                            header = TRUE,
                            sep = ';')

dados_ltv = read.csv('dados/cliente_ltv.csv', header = TRUE, sep = ';')

dados_seguro = read.csv('dados/cliente_seguros.csv',
                        header = TRUE,
                        sep = ';')


# Primeiro filtro de dados importantes das tabelas

{
  pessoas <- dados_pessoais %>% select(
    ESTADO,
    REGIAO,
    SEXO,
    PROFISSAO,
    IDADE,
    TEM_FILHOS,
    #SALARIO, APARECE NO FINANCEIRO
    N_DEPENDENTES,
    PROPRIETARIO_VEICULO,
    PROPRIETARIO_CASA,
    ESTADO_CIVIL
  ) %>%
    mutate(
      ESTADO = as.factor(ESTADO),
      REGIAO = as.factor(REGIAO),
      SEXO = as.factor(SEXO),
      PROFISSAO = as.factor(PROFISSAO),
      IDADE = as.factor(IDADE),
      TEM_FILHOS = as.factor(TEM_FILHOS),
      #SALARIO=as.factor(SALARIO), #APARECE NO FINANCEIRO
      N_DEPENDENTES = as.factor(N_DEPENDENTES),
      PROPRIETARIO_VEICULO = as.factor(PROPRIETARIO_VEICULO),
      PROPRIETARIO_CASA = as.factor(PROPRIETARIO_CASA),
      ESTADO_CIVIL = as.factor(ESTADO_CIVIL)
    )
  
  
  financeiro <- dados_financeiro %>% select(
    SALARIO,
    CC_SALDO_MEDIO,
    VALOR_FUNDOS_BANCARIOS,
    #CC__SALDO_ATUAL, # melhor considerar apenas o saldo medio
    VALOR_FIN_IMOBILIARIO,
    QTD_FIN_IMOBILIARIO,
    CREDIT_CARD_LIMITE
  ) %>%
    mutate(
      SALARIO = as.factor(SALARIO),
      CC_SALDO_MEDIO = as.factor(CC_SALDO_MEDIO),
      VALOR_FUNDOS_BANCARIOS = as.factor(VALOR_FUNDOS_BANCARIOS),
      #CC__SALDO_ATUAL=as.factor(CC__SALDO_ATUAL),
      VALOR_FIN_IMOBILIARIO = as.factor(VALOR_FIN_IMOBILIARIO),
      QTD_FIN_IMOBILIARIO = as.factor(QTD_FIN_IMOBILIARIO),
      CREDIT_CARD_LIMITE = as.factor(CREDIT_CARD_LIMITE)
    )
  
  seguro <- dados_seguro %>% select(TEM_SEG_IMOBILIARIO,
                                    TEMPO_CLIENTE_ANOS) %>%
    mutate(
      TEM_SEG_IMOBILIARIO = as.factor(TEM_SEG_IMOBILIARIO),
      TEMPO_CLIENTE_ANOS = as.factor(TEMPO_CLIENTE_ANOS)
    )
  
  
  ltv <- dados_ltv %>% select(#LTV, # a classe ja mostra o LTV
    CLASSE_LTV) %>%
    mutate(#LTV=as.factor(LTV),
      CLASSE_LTV = as.factor(CLASSE_LTV))
}


#removendo dados iniciais - apenas por organizaçao
remove(dados_pessoais)
remove(dados_financeiro)
remove(dados_ltv)
remove(dados_seguro)

