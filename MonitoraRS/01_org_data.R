# ---------------------------------------------------
# Carrega pacotes do R
# ---------------------------------------------------

library(tidyverse)
library(fs)
library(sf)
library(readxl)
library(janitor)
library(glue)
library(grid)
library(classInt)
library(magick)
library(lubridate)
library(ggthemes)
library(gganimate)

# ---------------------------------------------------
# Carrega dados e pre-processamento
# ---------------------------------------------------

dep <- read_sf(
  here::here("rs",
             "43MUE250GC_SIR.shp")) %>%
  clean_names() %>%
  st_set_crs(3857)

# ---------------------------------------------------
# Cria variáveis de aglomerações urbanas

rmpa <- c("Alvorada", "Araricá", "Arroio dos Ratos", "Cachoeirinha",
          "Campo Bom", "Canoas", "Capela de Santana", "Charqueadas",
          "Dois Irmãos", "Eldorado do Sul", "Estância Velha",
          "Esteio", "Glorinha", "Gravataí", "Guaíba", "Igrejinha",
          "Ivoti", "Montenegro", "Nova Hartz", "Nova Santa Rita",
          "Novo Hamburgo", "Parobé", "Portão", "Porto Alegre", 
          "Rolante", "Santo Antônio da Patrulha", "São Jerônimo",
          "São Leopoldo", "São Sebastião do Caí", "Sapiranga",
          "Sapucaia do Sul", "Taquara", "Triunfo",  "Viamão")

serra <- c("Antônio Prado", "Bento Gonçalves", "Carlos Barbosa",
           "Caxias do Sul", "Farroupilha", "Flores da Cunha",
           "Garibaldi", "Ipê", "Monte Belo do Sul", "Nova Pádua",
           "Pinto Bandeira", "Santa Tereza", "São Marcos")

litoral_n <- c("Arroio do Sal", "Balneário Pinhal", "Capão da Canoa",
               "Capivari", "Caraá", "Cidreira", "Dom Pedro de Alcântara",
               "Imbé", "Itati", "Mampituba", "Maquine", "Morrinhos do Sul",
               "Osório", "Palmares do Sul", "Terra de Areia", "Torres",
               "Tramandaí", "Três Cachoeiras", "Três Forquilhas", "Xangri-lá")

urba_sul <- c("Arroio do Padre", "Capão do Leão", "Pelotas",
              "Rio Grande", "São José do Norte")

rmpa      <- tolower(rmpa)
serra     <- tolower(serra)
litoral_n <- tolower(litoral_n)
urba_sul  <- tolower(urba_sul)

dep$rmpa      <- ifelse(tolower(iconv(x = dep$nm_municip, from = "latin1", to = "UTF-8")) %in% rmpa, 1, 0)
dep$serra     <- ifelse(tolower(iconv(x = dep$nm_municip, from = "latin1", to = "UTF-8")) %in% serra, 1, 0)
dep$litoral_n <- ifelse(tolower(iconv(x = dep$nm_municip, from = "latin1", to = "UTF-8")) %in% litoral_n, 1, 0)
dep$urba_sul  <- ifelse(tolower(iconv(x = dep$nm_municip, from = "latin1", to = "UTF-8")) %in% urba_sul, 1, 0)


# ---------------------------------------------------
# banco de dados de casos confirmados do RS:

covid <- readRDS(
  here::here("data",
             "casos_covid19_rs_mun.rds")) 

# filtrando o banco de dados para trabalhar só 
# com os casos confirmados: 

covid <- covid %>%
  select(city_ibge_code, date, confirmed, deaths)

# formatação correta das variáveis:
covid$date <- as.Date(covid$date, format = "%y/%m/%d")

covid$city_ibge_code <- as.character(covid$city_ibge_code)

dates <- unique(covid$date)

cd_mun <- dep$cd_geocodm

aux <- expand.grid(cd_mun, dates)
names(aux) <- c('cd_mun', 'dates')

# ---------------------------------------------------
# Casos acumulados por data de coleta
covid <- aux %>% 
  left_join(covid,
            by = c("cd_mun" = "city_ibge_code",  "dates" = "date")) %>% 
  mutate(confirmed = replace_na(confirmed, 0)) %>% 
  mutate(deaths = replace_na(deaths, 0)) %>% 
  rename(cd_municipio = cd_mun,  date = dates, casos = confirmed, mortes = deaths)

# Agrega por semana epidemiológica
covid_week <- covid %>% 
  group_by(time = epiweek(date), cd_municipio) %>%
  summarise(casos = sum(casos), mortes = sum(mortes))

aps