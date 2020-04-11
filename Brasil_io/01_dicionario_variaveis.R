# dicionário das variáveis: 

# criar uma função genérica que faz o dicionário 
# porém, é pelo menos necessário fornecer um vetor já com as descrições das variáveis 
# porque não tem como a função adivinhar o que cada variável significa 


dictionary_df <- function(dataset, descriptions){
  
  df <- as.data.frame(dataset) # confirmar que o banco é um data frame
  name_var <- names(df) # nome da variavel 
  type_var <- unlist(lapply(df, class)) # tipo da variável 
  dictionary <- data.frame(name_var, type_var, descriptions) # banco do dicionário
  names(dictionary) <- c('Nome da Variável', 'Tipo da Variável', 'Descrição da Variável')
  rownames(dictionary) <- 1:nrow(dictionary) # contagem do número de variáveis
  
  if(length(descriptions) != ncol(dataset)){
    print(paste0('O número descrições (', length(descriptions), 
                 ') é diferente da quantidade de variáveis no banco de dados (', 
                 ncol(dataset), ')'))
  } else {
    print(dictionary)
  }
  
}

descriptions1 <- c('Unidade Federativa',
                  'Data',
                  'Mortes por COVID-19 (Acumulado)',
                  'Mortes por Pneumonia em 2019 (Acumulado)',
                  'Mortes por Pneumonia em 2020 (Acumulado)',
                  'Mortes por Falha Respiratória em 2019 (Acumulado)',
                  'Mortes por Falha Respiratória em 2020 (Acumulado)',
                  'Semana Epidemiológica 2019',
                  'Semana Epidemiológica 2020',
                  'Mortes por COVID-19 (Diário)',
                  'Mortes por Pneumonia em 2019 (Diário)',
                  'Mortes por Pneumonia em 2020 (Diário)',
                  'Mortes por Falha Respiratória em 2019 (Diário)',
                  'Mortes por Falha Respiratória em 2020 (Diário)')

dictionary_df(df_obitos, descriptions1)


descriptions2 <- c('Unidade Federativa',
                   'Cidade',
                   'Data',
                   'Código do IBGE para a cidade',
                   'Casos de COVID-19 confirmados', 
                   'Casos de COVID-19 confirmados /por 100 mil habitantes', 
                   'Taxa de Mortalidade', 
                   'Quantidade de Mortes por COVID-19', 
                   'População estimada em 2019', 
                   'Se é a observação mais atual do lugar', 
                   'Ordenação das datas /por lugar', 
                   'Tipo de Lugar (City = Cidade; State = Estado/UF)')

dictionary_df(df_casos, descriptions2)
