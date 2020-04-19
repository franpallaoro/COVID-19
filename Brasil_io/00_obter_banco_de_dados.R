# --------------------------------------------------
# Obtenção dos dados obtidos em
# https://brasil.io/dataset/covid19/caso 
# pela API

# --------------------------------------------------
# Carrega pacotes

library(tidyverse)
library(httr)
library(jsonlite)

# -------------------------------------------------- 
# Coleta de dados (Web scraping) ÓBITOS

df_obitos <- NULL

cont <- 1 
path <- paste0(
  'https://brasil.io/api/dataset/covid19/obito_cartorio/data?page=', cont)
request <- GET(url = path)

while (request$status_code != 404) {
  response <- content(request, as = "text", encoding = "UTF-8")
  df <- fromJSON(response, flatten = TRUE)
  df_obitos <- rbind(df_obitos, df[["results"]])
  
  cont <- cont + 1 
  path <- paste0('https://brasil.io/api/dataset/covid19/obito_cartorio/data?page=', cont)
  request <- GET(url = path)
}

# -------------------------------------------------- 
# Preparação dos dados (data wrangling) ÓBITOS

# organizar o banco de dados de modo 
# que mostre todas as datas de um estado
# específico de uma vez (por ordem alfabética)
# e deixar o estado como a primeira variável 
# do banco de dados:

df_obitos <- df_obitos[order(df_obitos$state, df_obitos$date),]
df_obitos <- df_obitos[c('state', names(df_obitos)[!names(df_obitos) %in% 'state'])]


# transformando em data:
df_obitos$date <- as.Date(df_obitos$date, format = "%Y-%m-%d")



# -------------------------------------------------- 
# Coleta de dados (Web scraping) CASOS CONFIRMADOS

df_casos <- NULL

cont <- 1 
path <- paste0('https://brasil.io/api/dataset/covid19/caso/data?page=', cont)
request <- GET(url = path)

while (request$status_code != 404) {
  response <- content(request, as = "text", encoding = "UTF-8")
  df <- fromJSON(response, flatten = TRUE)
  df_casos <- rbind(df_casos, df[["results"]])
  
  cont <- cont + 1 
  path <- paste0('https://brasil.io/api/dataset/covid19/caso/data?page=', cont)
  request <- GET(url = path)
}

# -------------------------------------------------- 
# Preparação dos dados (data wrangling)  CASOS CONFIRMADOS
# mesmo esquema para o banco de casos
# neste caso, deixarei estado, cidade e data como as primeiras variáveis:

df_casos <- df_casos[order(df_casos$state, df_casos$city, df_casos$date),]
org_casos <- c('state', 'city', 'date')
df_casos <- df_casos[c(org_casos, names(df_casos)[!names(df_casos) %in% org_casos])]

# transformando em data:
df_casos$date <- as.Date(df_casos$date, format = "%Y-%m-%d")

# -------------------------------------------------- 
# Salva dados em um arquivo RDS

saveRDS(df_casos,
        file = here::here("casos_covid19_br_mun.rds"))

saveRDS(df_obitos,
        file = here::here("obitos_br_uf.rds"))

