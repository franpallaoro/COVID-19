library(tidyverse)
library(rgeos)
library(readxl)
library(sf)
library(jsonlite)
library(abjutils)

# esse script serve para organizar todos objetos de banco de dados que utilizarei no aplicativo
# são 3 principais:
# - o de casos do rs obtido através do brasil_io
# - dois shapefiles do rs(municipio e mesoregião) com dados sobre os casos de corona(confirmaodos, incidencia, mortes, etc)
# - um arquivo com latitudes e longitudes das cidades/hospitais e seus leitos
# - dois shapefiles do rs(municipio e mesoregião) com dados sobre os leitos

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


# fazendo o join dos dados covid ao shp

# shp municipio

dados_mapa_rs <- mapa_rs_shp %>%
  left_join(dados_covid_join, by = c("CD_GEOCMU" = "codigo")) %>%
  mutate(codigo = factor("CD_GEOCMU", levels = levels(mapa_rs_shp$CD_GEOCMU)))

# shp mesoregiao

dados_mapa_rs_meso <- mapa_meso_rs %>%
  left_join(dados_covid_join_meso, by = c("meso_regiao" = "mesorregiao"))


#################################
# lendo leitos UTI do site da SES
#################################

hospital_municipio <- read_excel("dados/leitos/outros/leitos_municipios.xlsx", col_names = F) %>%
  select(cnes = ...3, hospital = ...4, municipio = ...2) %>%
  mutate(municipio = str_to_title(municipio)) %>%
  left_join(codigos_cidades_sem_acento, by = "municipio") %>%
  select(-municipio) %>%
  left_join(codigos_cidades, by = "codigo") %>%
  select(-hospital)

write_csv(hospital_municipio, "dados/leitos/outros/hospital_municipio.csv")

hospital_municipio <- read_csv("dados/leitos/outros/hospital_municipio.csv") %>%
  mutate(codigo_ibge = as.character(codigo)) %>%
  select(-codigo)

dados_cnes <- read_csv("dados/leitos/outros/base_cnes_atualizada.csv") %>%
  select(CNES, LATITUDE, LONGITUDE)

pasta <- "dados/leitos/"
arquivos <- list.files(pasta, pattern = ".csv")
caminhos <- str_c(pasta, arquivos)

leitos_uti <- map(caminhos, read_csv) %>%
  map(dplyr::select, -(`Taxa Ocupação`)) %>%
  bind_rows() %>%
  distinct(`Cód`, `Últ Atualização`, .keep_all = T) %>%
  left_join(hospital_municipio, by = c("Cód" = "cnes")) %>%
  left_join(rs_mesoregiao_microregiao, by = c("codigo_ibge" = "codigo")) %>%
  mutate(data_atualizacao = lubridate::as_date(`Últ Atualização`, format = "%d/%m/%Y %H:%M")) %>%
  select(data_atualizacao = data_atualizacao, cnes = Cód, hospital = Hospital, codigo_ibge = codigo_ibge, municipio, leitos_internacoes = Pacientes, 
         leitos_total = Leitos, leitos_covid = Confirmados, meso_regiao = mesorregiao, data_hora_atualizacao = `Últ Atualização`) %>%
  mutate(codigo_ibge = factor(codigo_ibge, levels = levels(mapa_rs_shp$CD_GEOCMU))) %>%
  left_join(dados_cnes, by = c("cnes" = "CNES"))

leitos_join_mun <- leitos_uti %>%
  group_by(cnes) %>%
  filter(data_hora_atualizacao == max(data_hora_atualizacao)) %>%
  group_by(codigo_ibge) %>%
  summarise(leitos_internacoes = sum(leitos_internacoes), leitos_total = sum(leitos_total), leitos_covid = sum(leitos_covid))

leitos_join_meso <- leitos_uti %>%
  group_by(cnes) %>%
  filter(data_hora_atualizacao == max(data_hora_atualizacao)) %>%
  group_by(meso_regiao) %>%
  summarise(leitos_internacoes = sum(leitos_internacoes), leitos_total = sum(leitos_total), leitos_covid = sum(leitos_covid))
  

# fazendo o join dos dados covid ao shp

# shp municipio

leitos_mapa_mun_rs <- mapa_rs_shp %>%
  left_join(leitos_join_mun, by = c("CD_GEOCMU" = "codigo_ibge")) %>%
  mutate(codigo = factor("CD_GEOCMU", levels = levels(mapa_rs_shp$CD_GEOCMU)))

# shp mesoregiao

leitos_mapa_meso_rs <- mapa_meso_rs %>%
  left_join(leitos_join_meso, by = "meso_regiao")












