# ---------------------------------------------------
# Carrega pacotes do R

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

# ---------------------------------------------------
# Dados ministerio da saude:

df_MS <- readxl::read_excel(
  here::here("data", "MS.xlsx"))

summary_rs <- df_MS %>% 
  filter(estado == 'RS' & is.na(municipio) == 'TRUE' & data == max(df_MS$data))

df_MS <- df_MS %>% 
  mutate(cd_geocodm = as.character(df_MS$codmun)) %>%
  mutate(data = as.Date(df_MS$data)) %>% 
  filter(data == max(df_MS$data)) # ultimo dia

# ---------------------------------------------------
# Dados ministerio da saude - RS:

df_rs <- df_MS %>% 
  filter(estado == 'RS' & municipio != 'NA') %>%
  select(municipio, cd_geocodm, casosAcumulado, obitosAcumulado)

rm(df_MS)

# ---------------------------------------------------
# Dados municípios (população):

pop <- read.csv2(here::here("data", "Pop_RS.csv"))
names(pop)[2] <- 'municipio'
pop$Codigo <- as.character(pop$Codigo)

# ---------------------------------------------------
# merge população x dados covid:

covid <- pop %>% 
  left_join(df_rs) %>% 
  mutate(casosAcumulado = replace_na(casosAcumulado, 0)) %>% 
  mutate(obitosAcumulado = replace_na(obitosAcumulado, 0)) %>% 
  rename(casos = casosAcumulado, mortes = obitosAcumulado) %>% 
  mutate(casos = as.numeric(casos), mortes = as.numeric(mortes)) %>%
  select(municipio, Codigo, Estimativas.Populacionais.2018, casos, mortes) %>%
  rename(cd_geocodm = Codigo, pop = Estimativas.Populacionais.2018)

# ---------------------------------------------------
# df com geometry dos municipios:

dep <- read_sf(
  here::here("rs",
             "43MUE250GC_SIR.shp")) %>%
  clean_names() %>%
  st_set_crs(3857)

rm(pop)
rm(df_rs)

# ---------------------------------------------------
# banco de dados com as variaveis: 

popRS <- sum(covid$pop)

dep_week <- covid %>% 
  mutate(casos_p100 = casos/pop*100000) %>%
  mutate(letalidade = mortes/casos) %>%
  mutate(E = as.numeric(summary_rs$casosAcumulado)/popRS*pop) %>%
  mutate(SIR = casos/E) %>%
  select(municipio, cd_geocodm, pop, casos, mortes, casos_p100, letalidade, E, SIR)

dep_week$letalidade <- ifelse(is.na(dep_week$letalidade) == 'TRUE', 0, dep_week$letalidade)

#---------------------------------------------------
# criando variáveis categórias para os casos:

dep_week <- dep_week %>%
  mutate(casos_p100_cat = cut(x = casos_p100,
                              breaks = c(-Inf, 0, 5, 10, 20, 40, 80, Inf),
                              labels = c("0", "1 a 5", "5 a 10", "10 a 20", 
                                         "20 a 40", "40 a 80", "+80")),
         casos_cat = cut(x = casos,
                         breaks = c(-Inf, 0, 5, 10, 20, 40, 80, 160, Inf),
                         labels = c("0", "1 a 5", "6 a 10", "11 a 20", "21 a 40", 
                                    "41 a 80", "80 a 160", "+160")), 
         
         mortes_cat = cut(x = mortes,
                          breaks = c(-Inf, 0, 1, 2, 4, 8, 16, Inf),
                          labels = c("0", "1", "2", "3 a 4", "5 a 8", "9 a 16", "+16")), 
         
         letalidade_cat = cut(x = letalidade,
                              breaks = c(-Inf, 0, 0.025, 0.05, 0.1, 0.2, 0.5, Inf),
                              labels = c("0", "0 a 2.5%", "2.5% a 5%", "5% a 10%", 
                                         "10% a 20%", "20% a 50%", "+50%")), 
         
         SIR_cat = cut(x = SIR, 
                       breaks = c(-Inf, 0, 1, 3, 5, 7, 10, Inf), 
                       labels = c("0", "0 a 1", "1 a 3", "3 a 5", 
                                  "5 a 7", "7 a 10", "10+")))

#---------------------------------------------------
# banco de dados finalizado: 

dep_week <- dep %>%
  left_join(dep_week)

rm(covid)
rm(dep)
