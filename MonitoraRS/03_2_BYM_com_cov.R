#---------------------------
# pacotes utilizados:

library(ggplot2)
library(ggthemes)
library(SpatialEpi)
library(spdep)
library(dplyr)
library(INLA)

# chamando o banco de dados organizado:

rm(covid)
rm(covid_cumsum)
rm(covid_week)

# banco de dados:
temp <- dep_week %>% 
  filter(dep_week$time == max(dep_week$time, na.rm = TRUE)) 

# semana epid. mais atual:
temp <- temp %>%
  cbind(idarea = 1:nrow(temp))

rm(dep_week)

# banco com as covariáveis:
covars <- read_excel(here::here('data', 'DadosFEE_RS.xlsx'))
names(covars)[2] <- 'cd_geocodm'
covars$cd_geocodm <- as.character(covars$cd_geocodm)

# merge com a variável resposta: casos
dados <- temp %>%
  left_join(covars, by = 'cd_geocodm')

rm(covars)
rm(temp)

#-- lista de vizinhos de cada município
nb <- poly2nb(dados)
head(nb)

# PC priori:
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3)))

nb2INLA("map2.adj", nb)
g <- inla.read.graph(filename = "map2.adj")


formula <- casos ~ f(idarea, model = "bym2", graph = g, hyper = prior) + 
  Dist_POA + Densidade + Pop_IDOSA + Hospitais

res <- inla(formula,
            family = "poisson", data = dados,
            E = E, control.predictor = list(compute = TRUE))


dados$RR <- res$summary.fitted.values[, "mean"]
dados$LL <- res$summary.fitted.values[, "0.025quant"]
dados$UL <- res$summary.fitted.values[, "0.975quant"]

aux = as.data.frame(dados[,c('RR', 'UL', 'LL')])[1:3]

gRR <- ggplot() +
  geom_sf(data = dados, aes(fill = RR), size = .05) +
  theme_map() +
  labs(fill = 'RR', title = 'Risco Relativo') +
  theme(legend.position = "right") + 
  scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2], 
                      breaks = round(seq(0, ceiling(max(aux)), 3),2), 
                      limits = c(0, ceiling(max(aux)))) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

gUL <- ggplot() +
  geom_sf(data = dados, aes(fill = UL), size = .05) +
  theme_map() +
  labs(fill = 'UL', title = 'Intervalo de Credibilidade 97.5%') +
  theme(legend.position = "right") + 
  scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2], 
                      breaks = round(seq(0, ceiling(max(aux)), 3),2), 
                      limits = c(0, ceiling(max(aux)))) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

gLL <- ggplot() +
  geom_sf(data = dados, aes(fill = LL), size = .05) +
  theme_map() +
  labs(fill = 'LL', title = 'Intervalo de Credibilidade 2.5%') +
  theme(legend.position = "right") + 
  scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2], 
                      breaks = round(seq(0, ceiling(max(aux)), 3),2), 
                      limits = c(0, ceiling(max(aux)))) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

gridExtra::grid.arrange(gLL, gRR, gUL, ncol = 3, 
                        top = paste0("COVID-19 - Municípios RS - Semana Epidemiológica ", 
                                       unique(dados$time)))


#ggsave(filename = 'BYM.png', plot = gridExtra::grid.arrange(gLL, gRR, gUL, ncol = 3, 
#                                                            top = paste0("COVID-19 - Municípios RS - Semana Epidemiológica ", 
#                                                                         unique(dados$time))), 
#       path = 'C:/Users/Juliana/Downloads/series', width = 8, height = 8)
