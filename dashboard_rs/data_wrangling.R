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
  mutate(mesorregiao = str_remove(str_remove(str_to_title(NM_MESO),"\\sRio-Grandense"),"\\sDe Porto Alegre"))

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
  left_join(dados_covid_join_meso, by = c("mesorregiao" = "mesorregiao"))


#################################
# lendo leitos UTI do site da SES
#################################

hospital_municipio <- read_csv("dados/leitos/outros/hospital_municipio.csv") %>%
  mutate(codigo = as.factor(codigo_ibge))

dados_cnes <- read_csv("dados/leitos/outros/base_cnes_atualizada.csv") %>%
  select(CNES, LATITUDE, LONGITUDE)

pasta <- "dados/leitos/"
arquivos <- list.files(pasta, pattern = ".csv")
caminhos <- str_c(pasta, arquivos)

# arrumando problema da base deles em que em vez da coluna com dados de hospitais ter o nome "Hosital"
# ta com nome municipios(começou no dia 04/05/2020 e ta indo até hj 07/05/2020)
# ah e também a coluna Cód q possui o código cnes do hospital está escrita como Cód IBGE agr
# incrivel como sempre conseguem arranjar algum novo problema com esses dados da SES

arquivos_troca_nome <- c("leitos_dados_ses_05_05.csv","leitos_dados_ses_06_05.csv","leitos_dados_ses_07_05.csv",
                         "leitos_dados_ses_08_05.csv","leitos_dados_ses_09_05.csv","leitos_dados_ses_10_05.csv",
                         "leitos_dados_ses_11_05.csv")
caminhos_troca_nome <- str_c(pasta,arquivos_troca_nome)

arruma_nome <- map(caminhos_troca_nome, read_csv) %>%
  map(dplyr::mutate, Cód = `Cód IBGE`, Hospital = Município_1) %>%
  map(dplyr::select, -(`Taxa Ocupação`)) %>%
  bind_rows() 

caminhos <- caminhos[!(caminhos %in% caminhos_troca_nome)]

leitos_uti <- map(caminhos, read_csv) %>%
  map(dplyr::select, -(`Taxa Ocupação`)) %>%
  bind_rows() %>%
  bind_rows(arruma_nome) %>% # adicionando arquivos bugados
  left_join(hospital_municipio, by = c("Cód" = "cnes")) 

leitos_uti <- merge(leitos_uti, rs_mesoregiao_microregiao, by = "codigo", all = TRUE) %>%
  mutate(data_atualizacao = lubridate::as_date(`Últ Atualização`, format = "%d/%m/%Y",  tz = "America/Sao_Paulo"),
         Hospital = str_to_title(Hospital)) %>%
  distinct(`Cód`, data_atualizacao, .keep_all = T) %>%
  select(data_atualizacao = data_atualizacao, cnes = Cód, hospital = Hospital, codigo_ibge = codigo_ibge, municipio, leitos_internacoes = Pacientes,
         leitos_total = Leitos, leitos_covid = Confirmados, meso_regiao = mesorregiao, data_hora_atualizacao = `Últ Atualização`) %>%
  mutate(codigo_ibge = factor(codigo_ibge, levels = levels(mapa_rs_shp$CD_GEOCMU))) %>%
  left_join(dados_cnes, by = c("cnes" = "CNES")) %>%
  filter(data_atualizacao > "2020-04-27") %>%
  select(-data_hora_atualizacao) %>%
  arrange(data_atualizacao)

# resolvendo problema dos dados incompletos
# pegando dados de dias anteriores para os dias sem dado

# colocando as datas como se fosse colunas para verificar quais os dados que estão faltando para cada hospital

aux_total <- leitos_uti %>%
  select(-c(leitos_internacoes,leitos_covid)) %>%
  pivot_wider(names_from = data_atualizacao, values_from = leitos_total) %>%
  as.data.frame()

aux_internados <- leitos_uti %>%
  select(-c(leitos_total,leitos_covid)) %>%
  pivot_wider(names_from = data_atualizacao, values_from = leitos_internacoes) %>%
  as.data.frame()

aux_covid <- leitos_uti %>%
  select(-c(leitos_total,leitos_internacoes)) %>%
  pivot_wider(names_from = data_atualizacao, values_from = leitos_covid) %>%
  as.data.frame()

# e a partir dai caso um dado esteja sem atualização para aquele dia, então pega-se o dado do dia anterior
# foi bem hardcore mas acho que deu tudo certo, mas acho que seria importante avisar disso em algum momento,
# que estamos supondo a mesma situação do dia anterior para aqueles dias em que não temos dados(tudo a partir de quando começamos a coletar os dados no dia 28/04/20)

for (i in 9:ncol(aux_total)) {
  aux_total[,i] <- ifelse(is.na(aux_total[,i]),aux_total[,(i-1)],aux_total[,i])
  aux_internados[,i] <- ifelse(is.na(aux_internados[,i]),aux_internados[,(i-1)],aux_internados[,i])
  aux_covid[,i] <- ifelse(is.na(aux_covid[,i]),aux_covid[,(i-1)],aux_covid[,i])
}

aux_todos <- aux_total %>%
  pivot_longer(-names(aux_total)[1:7],names_to = "data_atualizacao", values_to = "leitos_total")

aux_internados <- aux_internados %>%
  pivot_longer(-names(aux_internados)[1:7],names_to = "data_atualizacao", values_to = "leitos_internacoes") %>%
  select(cnes,data_atualizacao,leitos_internacoes)

aux_covid <- aux_covid %>%
  pivot_longer(-names(aux_covid)[1:7],names_to = "data_atualizacao", values_to = "leitos_covid") %>%
  select(cnes,data_atualizacao,leitos_covid)

leitos_uti <- aux_todos %>%
  left_join(aux_internados, by = c("cnes","data_atualizacao")) %>%
  left_join(aux_covid, by = c("cnes","data_atualizacao")) %>%
  mutate(data_atualizacao = lubridate::as_date(data_atualizacao)) %>%
  mutate(lotacao = ifelse(leitos_total == 0, NA, leitos_internacoes/leitos_total),
         leitos_disponiveis = leitos_total - leitos_internacoes)


leitos_join_mun <- leitos_uti %>%
  group_by(cnes) %>%
  filter(data_atualizacao == max(data_atualizacao)) %>%
  group_by(codigo_ibge) %>%
  summarise(leitos_internacoes = sum(leitos_internacoes), leitos_total = sum(leitos_total), leitos_covid = sum(leitos_covid),
            lotacao = ifelse(sum(leitos_total)==0, NA, sum(leitos_internacoes)/sum(leitos_total)), leitos_disponiveis = leitos_total - leitos_internacoes)

names(leitos_uti)[5] = c("mesorregiao")

leitos_join_meso <- leitos_uti %>%
  group_by(cnes) %>%
  filter(data_atualizacao == max(data_atualizacao)) %>%
  group_by(mesorregiao) %>%
  summarise(leitos_internacoes = sum(leitos_internacoes), leitos_total = sum(leitos_total), leitos_covid = sum(leitos_covid),
            lotacao = ifelse(sum(leitos_total)==0, NA, sum(leitos_internacoes)/sum(leitos_total)), leitos_disponiveis = leitos_total - leitos_internacoes)
  

# fazendo o join dos dados covid ao shp

# shp municipio

leitos_mapa_mun_rs <- mapa_rs_shp %>%
  left_join(leitos_join_mun, by = c("CD_GEOCMU" = "codigo_ibge")) %>%
  mutate(codigo = factor("CD_GEOCMU", levels = levels(mapa_rs_shp$CD_GEOCMU)))

# shp mesoregiao

leitos_mapa_meso_rs <- mapa_meso_rs %>%
  left_join(leitos_join_meso, by = "mesorregiao")

# lendo arquivo com semana epidemoilógica para adicionar ao banco

semana <- read_csv("dados/semana_epidemio_dia.csv")

dados_covid_rs <- dados_covid_rs %>%
  left_join(semana, by = c("date" = "dia"))

# deixando só os objetos essenciais

rm(list=setdiff(ls(),c("leitos_mapa_mun_rs","leitos_mapa_meso_rs","leitos_uti","dados_mapa_rs_meso",
                       "dados_mapa_rs","dados_covid_rs")))





