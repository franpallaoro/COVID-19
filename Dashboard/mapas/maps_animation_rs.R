# ---------------------------------------------------
# Carrega pacotes do R
# ---------------------------------------------------

library(tidyverse)
# library(httr)
library(fs)
library(sf)
library(readxl)
library(janitor)
library(glue)
# library(tmap)
library(grid)
library(classInt)
library(magick)
# + btb, raster, fasterize, plyr
library(lubridate)
library(ggthemes)
library(gganimate)

# ---------------------------------------------------
# Carrega dados e pre-processa
# ---------------------------------------------------

# ---------------------------------------------------
# Dados municípios (shape file)

dep <- read_sf(
  here::here("data", "rs",
             "43MUE250GC_SIR.shp")) %>%
  clean_names() %>%
  st_set_crs(3857)

# Ajuste no código dos municípios 
dep$cd_geocodm <- substr(dep$cd_geocodm, 1, 6)

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
# Dados COVID-19 casos confirmados

covid <- read_csv(
  here::here("data",
             "SES_COVID19_2020-04-10_18-02-24.csv"))

names(covid) <- tolower(names(covid))

covid <- covid %>%
  group_by(cd_municipio, dt_coleta) %>% 
  summarise(casos = n())

covid$dt_coleta <- as.Date(covid$dt_coleta,
                           format = "%d/%m/%y")
covid$cd_municipio <- as.character(covid$cd_municipio)

dates <- unique(covid$dt_coleta)
cd_mun <- dep$cd_geocodm
aux <- expand_grid(cd_mun, dates)

covid <- aux %>% 
  left_join(covid,
            by = c("cd_mun" = "cd_municipio",  "dates" = "dt_coleta")) %>% 
  mutate(casos = replace_na(casos, 0)) %>% 
  rename(cd_municipio = cd_mun,  time = dates)
# 
# names(covid)[1:2] <- c("cd_municipio", "dt_coleta")

# Casos acumulados por data de coleta
covid_cum <- covid %>% 
  group_by(cd_municipio) %>% 
  arrange(time) %>% 
  mutate(casos = cumsum(casos))

# Agrega por semana epidemiológica

covid_week <- covid %>% 
  group_by(time = week(time), cd_municipio) %>%
  summarise(casos = sum(casos))

# Casos acumulados por data de coleta
covid_cum_week <- covid_week %>% 
  group_by(cd_municipio) %>% 
  arrange(time) %>% 
  mutate(casos = cumsum(casos))

# names(covid)[which(names(covid) == "dt_coleta")] <- "time"
# names(covid_week)[which(names(covid_week) == "week")] <- "time"
# 
# ---------------------------------------------------
# Dados municípios (população)

pop <- read_csv2(here::here("data", "Pop_RS.csv"))

names(pop) <- tolower(names(pop))

names(pop)[3] <- "pop"

# Ajuste no código dos municípios 
pop$codigo <- substr(pop$codigo, 1, 6)

# ---------------------------------------------------
# Pré-tratamento

date <- NULL
covid_df <- covid_cum_week

dep <- dep %>%
  left_join(pop, by = c("cd_geocodm" = "codigo"))

if (!is.null(date)) {
  dep <- dep %>% left_join(covid_df %>%
                             filter(time ==  date),
                           by = c("cd_geocodm" = "cd_municipio")) %>% 
    mutate(casos_p100 = (casos/pop) * 100000) %>% 
    mutate(casos_p100_cat = cut(x = casos_p100,
                                breaks = c(-Inf, 0, 5, 10, 20, 40, 80, Inf),
                                labels = c("0", "1 a 5", "6 a 10", "11 a 20", "21 a 40", "41 a 80", "80+")),
           casos_cat = cut(x = casos,
                           breaks = c(-Inf, 0, 5, 10, 20, 40, 80, 160, Inf),
                           labels = c("0", "1 a 5", "6 a 10", "11 a 20", "21 a 40", "41 a 80", "80 a 160", "160+")))
}else{
  dep <- dep %>% right_join(covid_df,
                            by = c("cd_geocodm" = "cd_municipio")) %>% 
    mutate(casos_p100 = (casos/pop) * 100000) %>% 
    mutate(casos_p100_cat = cut(x = casos_p100,
                                breaks = c(-Inf, 0, 5, 10, 20, 40, 80, Inf),
                                labels = c("0", "1 a 5", "6 a 10", "11 a 20", "21 a 40", "41 a 80", "80+")),
           casos_cat = cut(x = casos,
                           breaks = c(-Inf, 0, 5, 10, 20, 40, 80, 160, Inf),
                           labels = c("0", "1 a 5", "6 a 10", "11 a 20", "21 a 40", "41 a 80", "80 a 160", "160+")))
}

dep$casos[which(dep$cd_geocodm == "430000")] <- NA
dep$casos_cat[which(dep$cd_geocodm == "430000")] <- NA

virPallete <- viridis(n = 7)

# ----------------------------------
# Mapas

# Casos por 100 mil hab. RS

p <- ggplot() +
  geom_sf(data = subset(dep, subset = !is.na(casos_p100_cat)),
          aes(fill = casos_p100_cat), size = .05) +
  scale_fill_manual(values = virPallete) +
  # viridis::scale_fill_viridis(discrete = TRUE) +
  theme_map() +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "COVID-19 - Municípios RS - acumulado por semana epidemiológica") +
  theme(legend.position = "right") +
  facet_wrap( ~ time)

ggsave(plot = p,
       filename = here::here("outputs", "covid_rs_mun.png"),
       width = 8, height = 8)

# print(p)

covid_rs_vector <- ggplot() +
  geom_sf(data = dep,
          aes(fill = casos_p100), ) +
  viridis::scale_fill_viridis() +
  theme_map() +
  transition_time(time) +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "Semana epidemiológica: {round(frame_time, 0)}",
       caption = "Número  acumulado de casos confirmados COVID-19 - Municípios RS") +
  theme(legend.position = "right")

animate(covid_rs_vector)
anim_save(filename = "covid_rs_anima.gif",
          path = here::here("outputs"))

# ----------------------------------
# Casos por 100 mil hab. RMPA

dep_rmpa <- dep %>% 
  filter(rmpa == 1)

p <- ggplot() +
  geom_sf(data = subset(dep_rmpa, subset = !is.na(casos_p100_cat)),
          aes(fill = casos_p100_cat), size = .05) +
  scale_fill_manual(values = virPallete) +
  # viridis::scale_fill_viridis(discrete = TRUE) +
  theme_map() +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "COVID-19 - Municípios RMPA - acumulado por semana epidemiológica") +
  theme(legend.position = "right") +
  facet_wrap( ~ time)

ggsave(plot = p,
       filename = here::here("outputs", "covid_rmpa_mun.png"),
       width = 8, height = 8)

# print(p)

covid_rs_vector <- ggplot() +
  geom_sf(data = dep_rmpa,
          aes(fill = casos_p100), ) +
  viridis::scale_fill_viridis() +
  theme_map() +
  transition_time(time) +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "Semana epidemiológica: {round(frame_time, 0)}",
       caption = "Número  acumulado de casos confirmados COVID-19 - Municípios RMPA") +
  theme(legend.position = "right")

animate(covid_rs_vector)
anim_save(filename = "covid_rmpa_anima.gif",
          path = here::here("outputs"))

# ----------------------------------
# Casos por 100 mil hab. Serra

dep_serra <- dep %>% 
  filter(serra == 1)

p <- ggplot() +
  geom_sf(data = subset(dep_serra, subset = !is.na(casos_p100_cat)),
          aes(fill = casos_p100_cat), size = .05) +
  scale_fill_manual(values = virPallete) +
  # viridis::scale_fill_viridis(discrete = TRUE) +
  theme_map() +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "COVID-19 - Municípios Serra - acumulado por semana epidemiológica") +
  theme(legend.position = "right") +
  facet_wrap( ~ time)

ggsave(plot = p,
       filename = here::here("outputs", "covid_serra_mun.png"),
       width = 8, height = 8)

# print(p)

covid_rs_vector <- ggplot() +
  geom_sf(data = dep_serra,
          aes(fill = casos_p100), ) +
  viridis::scale_fill_viridis() +
  theme_map() +
  transition_time(time) +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "Semana epidemiológica: {round(frame_time, 0)}",
       caption = "Número  acumulado de casos confirmados COVID-19 - Municípios Serra") +
  theme(legend.position = "right")

animate(covid_rs_vector)
anim_save(filename = "covid_serra_anima.gif",
          path = here::here("outputs"))

# ----------------------------------
# Casos por 100 mil hab. Litoral Norte

dep_litoral_n <- dep %>% 
  filter(litoral_n == 1)

p <- ggplot() +
  geom_sf(data = subset(dep_litoral_n, subset = !is.na(casos_p100_cat)),
          aes(fill = casos_p100_cat), size = .05) +
  scale_fill_manual(values = virPallete) +
  # viridis::scale_fill_viridis(discrete = TRUE) +
  theme_map() +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "COVID-19 - Municípios Litoral Norte - acumulado por semana epidemiológica") +
  theme(legend.position = "right") +
  facet_wrap( ~ time)

ggsave(plot = p,
       filename = here::here("outputs", "covid_lit_norte_mun.png"),
       width = 8, height = 8)

# print(p)

covid_rs_vector <- ggplot() +
  geom_sf(data = dep_litoral_n,
          aes(fill = casos_p100), ) +
  viridis::scale_fill_viridis() +
  theme_map() +
  transition_time(time) +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "Semana epidemiológica: {round(frame_time, 0)}",
       caption = "Número  acumulado de casos confirmados COVID-19 - Municípios Litoral Norte") +
  theme(legend.position = "right")

animate(covid_rs_vector)
anim_save(filename = "covid_lit_norte_anima.gif",
          path = here::here("outputs"))

# ----------------------------------
# Casos por 100 mil hab. Litoral Norte

dep_urba_sul <- dep %>% 
  filter(urba_sul == 1)

p <- ggplot() +
  geom_sf(data = subset(dep_urba_sul, subset = !is.na(casos_p100_cat)),
          aes(fill = casos_p100_cat), size = .05) +
  scale_fill_manual(values = virPallete) +
  # viridis::scale_fill_viridis(discrete = TRUE) +
  theme_map() +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "COVID-19 - Municípios Urbana Sul - acumulado por semana epidemiológica") +
  theme(legend.position = "right") +
  facet_wrap( ~ time)

ggsave(plot = p,
       filename = here::here("outputs", "covid_urba_sul_mun.png"),
       width = 8, height = 8)

# print(p)

covid_rs_vector <- ggplot() +
  geom_sf(data = dep_urba_sul,
          aes(fill = casos_p100), ) +
  viridis::scale_fill_viridis() +
  theme_map() +
  transition_time(time) +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "Semana epidemiológica: {round(frame_time, 0)}",
       caption = "Número  acumulado de casos confirmados COVID-19 - Municípios Urbana Sul") +
  theme(legend.position = "right")

animate(covid_rs_vector)
anim_save(filename = "covid_urba_sul_anima.gif",
          path = here::here("outputs"))

# ----------------------------------
# Casos (absoluto)

p <- ggplot() +
  geom_sf(data = subset(dep, subset = !is.na(casos_cat)),
          aes(fill = casos_cat)) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme_map() +
  labs(fill = "Casos confirmados",
       title = "COVID-19 - Municípios RS - acumulado por semana epidemiológica") +
  theme(legend.position = "right") +
  facet_wrap( ~ time)

ggsave(plot = p,
       filename = here::here("outputs", "covid_rs_mun_absoluto.png"),
       width = 8, height = 8)

# print(p)

covid_rs_vector <- ggplot() +
  geom_sf(data = dep,
          aes(fill = casos)) +
  viridis::scale_fill_viridis() +
  theme_map() +
  transition_time(time) +
  labs(fill = "Casos confirmados",
       title = "Semana epidemiológica: {round(frame_time, 0)}",
       caption = "Número acumulado de casos confirmados COVID-19 - Municípios RS") +
  theme(legend.position = "right")

animate(covid_rs_vector)
anim_save(filename = "covid_rs_anima_absoluto.gif",
          path = here::here("outputs"))

# p_anima <- animate(covid_rs_vector)

covid_rs_vector <- ggplot() +
  geom_sf(data = dep,
          aes(fill = casos_cat)) +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme_map() +
  transition_times(time) +
  labs(fill = "Casos confirmados",
       title = "Semana epidemiológica: {round(frame_time, 0)}",
       caption = "Número acumulado de casos confirmados COVID-19 - Municípios RS") +
  theme(legend.position = "right")

animate(covid_rs_vector)
anim_save(filename = "covid_rs_anima_absoluto.gif",
          path = here::here("outputs"))

# ----------------------------------
# Gráficos
# ----------------------------------

covid_cum_rs <- covid_cum %>% 
  group_by(time) %>% 
  summarise(casos = sum(casos))
covid_cum_rs$casos_inc <- c(covid_cum_rs$casos[1], diff(x = covid_cum_rs$casos, lag = 1))

p1 <- ggplot(data = covid_cum_rs,
             mapping = aes(x = time, y = casos)) +
  geom_point(color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1]) +
  geom_line(color = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1]) +
  labs(x = "Tempo (dias)", y = "Número acumulado de casos confirmados") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p2 <- p1 + scale_y_log10() +
  geom_smooth(method = "lm", se = F, colour = "black") +
  labs(x = "Tempo (dias)", y = "Número de casos confirmados (log-escala)")

p3 <- ggplot(data = covid_cum_rs, aes(x = time,
                            y = casos_inc)) + 
  geom_bar(stat = "identity",
           fill = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1]) + 
  labs(x = "Tempo (dias)", y = "Incidência incremental diária") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

p4 <- ggplot(data = covid_cum_rs, aes(x = time, y = casos)) + 
  geom_bar(stat = "identity",
           fill = RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1]) + 
  labs(x = "Tempo (dias)", y = "Incidência incremental diária") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

library(incidence)
library(distcrete)
library(epitrix)
library(projections)
library(EpiEstim)
library(earlyR)

# infectados

onset <- read_csv(
  here::here("data",
             "SES_COVID19_2020-04-10_18-02-24.csv"))

onset <- as.Date(onset$DT_COLETA,
                 format = "%d/%m/%y")

i <- incidence(onset)
plot(i) # full outbreak

# Serial interval
mu <- 4.80 #6.48 #4.80
sigma <- 2.70 #3.83 #2.70
cv <- sigma / mu
params <- gamma_mucv2shapescale(mu, cv)
params

si <- distcrete("gamma", shape = params$shape,
                scale = params$scale,
                interval = 1, w = 0)
si

plot(1:30, si$d(1:30), type = "h", lwd = 3, col = "navy",
     main = "Serial interval", xlab = "Days after onset",
     ylab = "Relative infectiousness")


res <- estimate_R(incid = i[1:30], method = "parametric_si",
                  config = make_config(list(mean_si = mu, std_si = sigma)))
plot(res, "R") + theme_bw() + labs(x = "Tempo (dias)") + ggtitle("R estimado")

res <- get_R(i[1:30], si_mean = mu, si_sd = sigma)
res

plot(res)

plot(res, "lambdas", scale = length(onset) + 1)
abline(v = onset, lwd = 3, col = "grey")
abline(v = today, col = "blue", lty = 2, lwd = 2)
points(onset, seq_along(onset), pch = 20, cex = 3)

R_val <- sample_R(res, 1000)
summary(R_val)
quantile(R_val)

hist(R_val, border = "white", col = "steelblue",
     xlab = "Values of R",
     main = "Sample of likely R values")

set.seed(11)
pred <- project(i[1:30], R = R_val, si = res$si, n_days = 30, n_sim = 1000)
pred

plot(pred) # default plot

pred_cum <- cumulate(pred) # cumulative predictions
plot(pred_cum) # plot cumulative predictions

apply(pred, 1, mean) # average prediction per day
apply(pred, 1, range) # range across simulations

plot(i) %>% add_projections(pred, boxplots = FALSE)
