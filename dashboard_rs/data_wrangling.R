library(tidyverse)
library(rgeos)
library(readxl)
library(sf)
library(jsonlite)


# lendo shapefiles RS

mapa_rs_shp <- sf::st_read("dados/shapefiles/43MUE250GC_SIR.shp", quiet = TRUE) %>%
  mutate(municipio = str_to_title(NM_MUNICIP))

mapa_rs_shp[mapa_rs_shp$municipio=="Westfalia","municipio"] <- "Westfália"
mapa_rs_shp[mapa_rs_shp$municipio=="Vespasiano Correa","municipio"] <- "Vespasiano Corrêa"

# criando um objeto para atribuir códigos ibge às cidades

codigos_cidades <- tibble(municipio = mapa_rs_shp$municipio, codigo = mapa_rs_shp$CD_GEOCMU)

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
  select(date,confirmed,deaths,is_last,codigo,municipio,confirmed_per_100k_inhabitants,death_rate,place_type)
  

dados_covid_join <- dados_covid_rs %>%
  filter(is_last) %>%
  filter(place_type=="city") %>%
  select(-municipio)

# lendo mesoregiões

rs_mesoregiao_microregiao <- read_csv("dados/mesoregiao/rs_mesoregiao_microregiao.csv") %>%
  mutate(municipio = str_to_title(municipio),
         microregiao = str_to_title(municipio)) %>%
  left_join(codigos_cidades, by = "municipio") %>%  # atribuindo o código
  select(-municipio)


# lendo equipamentos mantenedores de vida

# nesses próximos dois bancos a ideia seria juntá-los ao banco principal peo código ibge do municipio, mas o código q vem
# é de 6 dígitos e não serve, portanto eu adicionei o código pelo nome do municipio e dps dai sim faço o join pelo código

equipamento_vida <- read_delim("dados/equipamentos/A191154168_181_165_30.csv", ";", escape_double = FALSE, 
                               locale = locale(encoding = "WINDOWS-1252"),trim_ws = TRUE, skip = 4) %>%
  filter(!(is.na(Equipamentos_Existentes)|`Município` == "Total")) %>%
  separate(Município, into = c("codigo_ruim", "municipio"), sep = 7) %>%
  mutate(municipio = ifelse(municipio == "Westfalia", "Westfália", str_to_title(municipio))) %>%
  left_join(codigos_cidades, by = "municipio") %>%
  select(codigo, equip_vida = Equipamentos_Existentes)


# lendo equipamentos(respirador/ventilador)

respirador_ventilador <- read_delim("dados/equipamentos/A185831168_181_165_30.csv", ";", escape_double = FALSE, 
                                   locale = locale(encoding = "WINDOWS-1252"),trim_ws = TRUE, skip = 4) %>%
  filter(!(is.na(Equipamentos_Existentes)|`Município` == "Total")) %>%
  separate(Município, into = c("codigo_ruim", "municipio"), sep = 7) %>%
  mutate(municipio = ifelse(municipio == "Westfalia", "Westfália", str_to_title(municipio))) %>%
  left_join(codigos_cidades, by = "municipio") %>%
  select(codigo, ventilador_mecanico = Equipamentos_Existentes)


# lendo leitos UTI

leitos_uti <- read_delim("dados/leitos/A160924168_181_165_30.csv",";", escape_double = FALSE,
                         locale = locale(encoding = "WINDOWS-1252"),trim_ws = TRUE, skip = 3) %>%
  filter(!(is.na(Total)|`Município` == "Total")) %>%
  separate(Município, into = c("codigo_ruim", "municipio"), sep = 7) %>%
  mutate(municipio = ifelse(municipio == "Westfalia", "Westfália", str_to_title(municipio))) %>%
  left_join(codigos_cidades, by = "municipio") %>%
  mutate_all(~ replace(., . == "-", 0)) %>%
  mutate(uti_adulto = as.numeric(`UTI adulto I`)+as.numeric(`UTI adulto II`)+as.numeric(`UTI adulto III`),
         uti_pediatrico = as.numeric(`UTI pediátrica II`)+as.numeric(`UTI pediátrica III`)) %>%
  select(codigo, uti_adulto, uti_pediatrico)


# juntando em um banco só

banco <- rs_mesoregiao_microregiao %>%
  left_join(equipamento_vida, by = "codigo") %>%
  left_join(respirador_ventilador, by = "codigo") %>%
  left_join(dados_covid_join, by = "codigo") %>%
  left_join(leitos_uti, by = "codigo") %>%
  mutate(codigo = factor(codigo, levels = levels(mapa_rs_shp$CD_GEOCMU)))
  

dados_mapa_rs <- mapa_rs_shp %>%
  left_join(banco, by = c("CD_GEOCMU" = "codigo"))


