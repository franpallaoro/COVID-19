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
# Separando os dados e pegando somente os do RS:

casosRS <- df_casos %>%
  filter(state == 'RS' & place_type == 'city')

# -------------------------------------------------- 
# Salva dados em um arquivo RDS

saveRDS(casosRS,
        file = here::here("data", "casos_covid19_rs_mun.rds"))

# -------------------------------------------------- 
# Separando os dados e pegando somente os do RS total:

casosRS <- df_casos %>%
  filter(state == 'RS' & place_type == 'state')

# -------------------------------------------------- 
# Salva dados em um arquivo RDS

saveRDS(casosRS,
        file = here::here("data", "casos_covid19_rs_tot.rds"))

rm(df)
rm(df_casos)
rm(request)
rm(cont)
rm(org_casos)
rm(path)
rm(response)
rm(casosRS)
