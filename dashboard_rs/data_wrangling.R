library(tidyverse)
library(rgeos)
library(readxl)
library(sf)
library(jsonlite)
library(abjutils)

# esse script serve para organizar todos objetos de banco de dados que utilizarei no aplicativo
# são 3 principais:
# - o de casos do rs obtido através do brasil_io
# - o shapefile do rs com dados sobre os casos de corona(confirmaodos, incidencia, mortes, etc)
# - um arquivo com latitudes e longitudes das cidades/hospitais e seus leitos

# lendo shapefiles RS municipios

mapa_rs_shp <- sf::st_read("dados/shapefiles/43MUE250GC_SIR.shp", quiet = TRUE) %>%
  mutate(municipio = str_to_title(NM_MUNICIP))

mapa_rs_shp[mapa_rs_shp$municipio=="Westfalia","municipio"] <- "Westfália"
mapa_rs_shp[mapa_rs_shp$municipio=="Vespasiano Correa","municipio"] <- "Vespasiano Corrêa"

# lendo shapefiles mesoregiões RS

mapa_meso_rs <- sf::st_read("dados/shapefiles/43MEE250GC_SIR.shp", quiet = TRUE) %>%
  mutate(meso_regiao = str_remove(str_remove(str_to_title(NM_MESO),"\\sRio-Grandense"),"\\sDe Porto Alegre"))

# criando um objeto para atribuir códigos ibge às cidades

codigos_cidades <- tibble(municipio = mapa_rs_shp$municipio, codigo = mapa_rs_shp$CD_GEOCMU)

codigos_cidades_sem_acento <- codigos_cidades %>%
  mutate(municipio = rm_accent(municipio))

# lendo mesoregiões

rs_mesoregiao_microregiao <- read_csv("dados/mesoregiao/rs_mesoregiao_microregiao.csv") %>%
  mutate(municipio = str_to_title(municipio),
         mesorregiao = str_to_title(mesorregiao)) %>%
  left_join(codigos_cidades, by = "municipio") %>%  # atribuindo o código
  select(-municipio)

# lendo dados brasil.io

dados_brasil_io <- NULL
dados_brasil_io <- read_csv("https://brasil.io/dataset/covid19/caso?format=csv")

if(is.null(dados_brasil_io)) {
  dados_brasil_io <- read_csv("dados/covid/brasil.io_reserva.csv")
} else {
  write_csv(dados_brasil_io,"dados/covid/brasil.io_reserva.csv")
}

dados_covid_rs <- dados_brasil_io %>%
  mutate(date = as.Date(date)) %>%
  filter(state == "RS") %>%
  mutate(municipio = str_to_title(city)) %>%
  mutate(codigo = factor(city_ibge_code, levels = levels(codigos_cidades$codigo))) %>%
  left_join(rs_mesoregiao_microregiao, by = "codigo") %>%
  select(date,confirmed,deaths,is_last,codigo,municipio,confirmed_per_100k_inhabitants,death_rate,place_type,mesorregiao,estimated_population_2019)
  

dados_covid_join <- dados_covid_rs %>%
  filter(is_last) %>%
  filter(place_type=="city") %>%
  select(-municipio)

dados_covid_join_meso <- dados_covid_join %>%
  group_by(mesorregiao) %>%
  summarise(confirmed = sum(confirmed), deaths = sum(deaths), estimated_population_2019 = sum(estimated_population_2019),
            death_rate = sum(deaths)/sum(confirmed), confirmed_per_100k_inhabitants = sum(confirmed)*100000/sum(estimated_population_2019))

#################################
# lendo leitos UTI do site da SES
#################################


lista_municipos <- read_csv("dados/shapefiles/municipios.csv") %>%
  select(codigo_ibge, latitude, longitude) %>%
  mutate(codigo_ibge = factor(codigo_ibge))

locais <- c("HOSPITAL NOSSA SENHORA DA CONCEICAO SA","HOSPITAL DE CLINICAS","	HOSPITAL DIVINA PROVIDENCIA","HOSPITAL ERNESTO DORNELLES",
            "HOSPITAL MAE DE DEUS","HOSPITAL MOINHOS DE VENTO","HOSPITAL RESTINGA E EXTREMO SUL","IRMANDADE DA SANTA CASA DE MISERICORDIA DE PORTO ALEGRE",
            "HOSPITAL SAO LUCAS DA PUCRS","HOSPITAL CRISTO REDENTOR","HPS","HOSPITAL FEMINA","HOSPITAL INDEPENDENCIA",
            "HOSPITAL MATERNO INFANTIL PRESIDENTE VARGAS","AESC HOSPITAL SANTA ANA","ASSOCIACAO HOSPITALAR VILA NOVA",
            "INSTITUTO DE CARDIOLOGIA","HOSPITAL BDW","HOSPITAL BENEFICENCIA PORTUGUESA","HBMPA")


latitude <- c(-30.015870,-30.038435,-30.084428,-30.047509,-30.058845,-30.025520,-30.142524,-30.030803,
              -30.055122,-30.010102,-30.036874,-30.029239,-30.061051,-30.029525,-30.086664,-30.119308,
              -30.049092,-30.045729,-30.028964,-30.09793)
longitude <- c(-51.158236,-51.206637,-51.188390,-51.212131,-51.228945,-51.208420,-51.128803,-51.221512,
               -51.173819,-51.159260,-51.209282,-51.206848,-51.149073,-51.214865,-51.206890,-51.207744,
               -51.209030,-51.208110,-51.218389,-51.25114)

lat_long <- tibble(
  local = locais,
  lat = latitude,
  long = longitude
)

pasta <- "dados/leitos/"
arquivos <- list.files(pasta, pattern = ".xlsx")
caminhos <- str_c(pasta, arquivos)

leitos_uti <- map(caminhos, read_excel, na = c("","-")) %>%
  bind_rows() %>%
  distinct(CNES, DATA, .keep_all = T) %>%
  mutate(covid_confirmado = `TOTAL...9`) %>%
  mutate(municipio = str_to_title(MUNICIPIO)) %>%
  left_join(codigos_cidades_sem_acento, by = "municipio") %>%
  select(-municipio) %>%
  left_join(codigos_cidades, by = "codigo") %>%
  select(data = DATA, cnes = CNES, hospital = HOSPITAL, codigo, municipio, leitos_internacoes = INTERNACOES, 
         leitos_total = LEITOS, leitos_covid = covid_confirmado) %>%
  left_join(lista_municipos, by = c("codigo" = "codigo_ibge")) %>%
  mutate(codigo = factor(codigo, levels = levels(mapa_rs_shp$CD_GEOCMU)))

# fazendo o join dos dados covid ao shp

# shp municipio

dados_mapa_rs <- mapa_rs_shp %>%
  left_join(dados_covid_join, by = c("CD_GEOCMU" = "codigo")) %>%
  mutate(codigo = factor("CD_GEOCMU", levels = levels(mapa_rs_shp$CD_GEOCMU)))

# shp mesoregiao

dados_mapa_rs_meso <- mapa_meso_rs %>%
  left_join(dados_covid_join_meso, by = c("meso_regiao" = "mesorregiao"))

