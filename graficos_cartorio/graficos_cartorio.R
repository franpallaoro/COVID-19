library(tidyverse)
library(RColorBrewer)
library(plotly)

obitos_cartorio <- read_csv("Brasil_io/obitos_cartorio.csv")

names(obitos_cartorio) <- c("X1","Estado","Data","Acumulado mortes covid19","Acumulado mortes Pneumonia 2019","Acumulado mortes Pneumonia 2020",
                            "Acumulado mortes por falha respiratória 2019","Acumulado mortes por falha respiratória 2020","Semana_epidemiologica_2019",
                            "Semana_epidemiologica_2020","Mortes covid19","Mortes Pneumonia 2019","Mortes Pneumonia 2020",
                            "Mortes por falha respiratória 2019","Mortes por falha respiratória 2020")

#########
# gráfico de novas mortes por data para o brasil itneiro
########

aux <- obitos_cartorio %>%
  group_by(Data, Semana_epidemiologica_2019,Semana_epidemiologica_2020) %>%
  summarise_at(names(obitos_cartorio)[c(4:8,11:15)],sum) %>%
  pivot_longer(
    cols = -c(Data, Semana_epidemiologica_2019,Semana_epidemiologica_2020),
    names_to = "disease_type",
    values_to = "deaths"
  ) %>%
  filter(!str_detect(disease_type,"^Acumulado"))

valores <- levels(as.factor(aux$disease_type))
paleta <- brewer.pal(n = 5, name = "Dark2")

names(paleta) <- valores

p1 <- ggplot(aux,aes(x=Data,y=deaths, color = disease_type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(name = "Doenças", values = paleta) +
  labs(x = "Dia do ano",y = "Número de óbitos diários") 

p1

ggplotly(p1)

ggsave("graficos_cartorio/plot_data_novas_mortes_brasil.png",p1)

###########
# gráfico de novas mortes por data para o brasil itneiro
###########

aux <- obitos_cartorio %>%
  group_by(Semana_epidemiologica_2020) %>%
  summarise_at(names(obitos_cartorio)[c(4:8,11:15)],sum) %>%
  pivot_longer(
    cols = -c(Semana_epidemiologica_2020),
    names_to = "disease_type",
    values_to = "deaths"
  ) %>%
  filter(!str_detect(disease_type,"^Acumulado"))

valores <- levels(as.factor(aux$disease_type))
paleta <- brewer.pal(n = 5, name = "Dark2")

names(paleta) <- valores

p2 <- ggplot(aux,aes(x=Semana_epidemiologica_2020,y=deaths, color = disease_type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(name = "Doenças", values = paleta) +
  labs(x = "Semana Epidemiológica",y = "Número de óbitos diários") 

p2

ggplotly(p2)

ggsave("graficos_cartorio/plot_semana_epidemio_novas_mortes_brasil.png",p2)

################
# gráfico de novas mortes por data para estado específico
################

estado <- "RS" # input do usuário

aux <- obitos_cartorio %>%
  group_by(Data, Estado,Semana_epidemiologica_2019,Semana_epidemiologica_2020) %>%
  filter(Estado == estado) %>%
  summarise_at(names(obitos_cartorio)[c(4:8,11:15)],sum) %>%
  pivot_longer(
    cols = -c(Data, Estado,Semana_epidemiologica_2019,Semana_epidemiologica_2020),
    names_to = "disease_type",
    values_to = "deaths"
  ) %>%
  filter(!str_detect(disease_type,"^Acumulado"))

valores <- levels(as.factor(aux$disease_type))
paleta <- brewer.pal(n = 5, name = "Dark2")

names(paleta) <- valores

p3 <- ggplot(aux,aes(x=Data,y=deaths, color = disease_type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(name = "Doenças", values = paleta) +
  labs(x = "Dia do ano",y = "Número de óbitos diários") 

p3

ggplotly(p3)

ggsave("graficos_cartorio/plot_data_novas_mortes_rs.png",p3)


################
# gráfico de novas mortes por semana epdiemiológica para estado específico
################

estado <- "RS" # input do usuário

aux <- obitos_cartorio %>%
  group_by(Estado,Semana_epidemiologica_2020) %>%
  filter(Estado == estado) %>%
  summarise_at(names(obitos_cartorio)[c(4:8,11:15)],sum) %>%
  pivot_longer(
    cols = -c(Estado,Semana_epidemiologica_2020),
    names_to = "disease_type",
    values_to = "deaths"
  ) %>%
  filter(!str_detect(disease_type,"^Acumulado"))

valores <- levels(as.factor(aux$disease_type))
paleta <- brewer.pal(n = 5, name = "Dark2")

names(paleta) <- valores

p4 <- ggplot(aux,aes(x=Semana_epidemiologica_2020,y=deaths, color = disease_type)) +
  geom_line() +
  geom_point() +
  scale_color_manual(name = "Doenças", values = paleta) +
  labs(x = "Semana Epidemiológica",y = "Número de óbitos diários") 

p4

ggplotly(p4)

ggsave("graficos_cartorio/plot_semana_epidemio_novas_mortes_rs.png",p4)

















