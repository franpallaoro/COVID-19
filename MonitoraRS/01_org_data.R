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

dep$rmpa      <- ifelse(tolower(iconv(x = dep$nm_municip, 
                                      from = "latin1", to = "UTF-8")) %in% rmpa, 1, 0)
dep$serra     <- ifelse(tolower(iconv(x = dep$nm_municip, 
                                      from = "latin1", to = "UTF-8")) %in% serra, 1, 0)
dep$litoral_n <- ifelse(tolower(iconv(x = dep$nm_municip, 
                                      from = "latin1", to = "UTF-8")) %in% litoral_n, 1, 0)
dep$urba_sul  <- ifelse(tolower(iconv(x = dep$nm_municip, 
                                      from = "latin1", to = "UTF-8")) %in% urba_sul, 1, 0)


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

# ---------------------------------------------------
# Dados municípios (população)

pop <- read.csv2(here::here("data", "Pop_RS.csv"))
names(pop) <- tolower(names(pop))

pop$codigo <- as.character(pop$codigo)

# ---------------------------------------------------
# Pré tratamento
#---------------------------------------------------

covid_week <- covid_week %>% 
  right_join(pop, by = c("cd_municipio" = "codigo"))

covid_week <- covid_week %>% 
  mutate(casos_p100 = (casos/estimativas.populacionais.2018) * 100000) %>%
  mutate(letalidade = mortes/casos) %>%
  select(time, municipios, cd_municipio, casos, mortes, casos_p100, letalidade)

#---------------------------------------------------
# tirando os NA de letalidade:

covid_week$letalidade <- ifelse(is.na(covid_week$letalidade) == 'TRUE', 
                                0, covid_week$letalidade)

#---------------------------------------------------
# criando variáveis categórias para os casos:

covid_week <- covid_week %>%
  mutate(casos_p100_cat = cut(x = casos_p100,
                              breaks = c(-Inf, 0, 10, 20, 50, 80, 100, 150, Inf),
                              labels = c("0", "1 a 10", "11 a 20", "21 a 50", 
                                         "51 a 80", "81 a 100", "101 a 150", "150+")),
         casos_cat = cut(x = casos,
                         breaks = c(-Inf, 0, 10, 20, 40, 80, 100, 200, Inf),
                         labels = c("0", "1 a 10", "11 a 20", "21 a 40", 
                                    "41 a 80", "81 a 100", "101 a 200", "200+")),
         mortes_cat = cut(x = mortes,
                         breaks = c(-Inf, 0, 5, 10, 15, 20, 30, 35, Inf),
                         labels = c("0", "1 a 5", "6 a 10", "11 a 15", 
                                    "16 a 20", "21 a 30", "31 a 35", "35+")), 
         
         letalidade_cat = cut(x = letalidade,
                          breaks = c(-Inf, 0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, Inf),
                          labels = c("0", "0 a 5%", "5% a 10%", "10% a 20%", 
                                     "20% a 30%", "30% a 40%", "40% a 50%", "+50%")))

#---------------------------------------------------
# banco de dados final:

dep_week <- dep %>%
  left_join(covid_week, by = c("cd_geocodm" = "cd_municipio"))

dep_date <- dep %>%
  left_join(covid, by = c("cd_geocodm" = "cd_municipio"))