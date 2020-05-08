#---------------------------
# pacotes utilizados:

library(ggplot2)
library(ggthemes)
library(SpatialEpi)
library(spdep)
library(dplyr)
library(INLA)

# modelo BYM sem covariáveis, podendo ser feito para cada semana epidemiologica disponível: 

BYM2_semana_ep <- function(sem_ep){
  
  temp <- dep_week %>% 
    filter(dep_week$time == sem_ep)
  
  temp <- temp %>%
    cbind(idarea = 1:nrow(temp))
  
  nb <- poly2nb(temp)

  # PC priori:
  prior <- list(
    prec = list(
      prior = "pc.prec",
      param = c(0.5 / 0.31, 0.01)),
    phi = list(
      prior = "pc",
      param = c(0.5, 2 / 3)))
  
  nb2INLA("map.adj", nb)
  g <- inla.read.graph(filename = "map.adj")
  
  formula <- casos ~ f(idarea, model = "bym2", graph = g, hyper = prior)
  
  res <- inla(formula,
              family = "poisson", data = temp,
              E = E, control.predictor = list(compute = TRUE))
  
  temp$RR <- res$summary.fitted.values[, "mean"]
  temp$LL <- res$summary.fitted.values[, "0.025quant"]
  temp$UL <- res$summary.fitted.values[, "0.975quant"]
  
  
  gRR <- ggplot() +
    geom_sf(data = temp, aes(fill = RR), size = .05) +
    theme_map() +
    labs(fill = 'RR', title = paste0("COVID-19 - Municípios RS - Risco Relativo - 
                                     Semana Epidemiológica ", unique(temp$time))) +
    theme(legend.position = "right") + 
    scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2]) + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
  
  gUL <- ggplot() +
    geom_sf(data = temp, aes(fill = UL), size = .05) +
    theme_map() +
    labs(fill = 'UL', title = paste0("COVID-19 - Municípios RS - Intervalo de Credibilidade 97.5% - 
       Semana Epidemiológica ", unique(temp$time))) +
    theme(legend.position = "right") + 
    scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2]) + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
  
  gLL <- ggplot() +
    geom_sf(data = temp, aes(fill = LL), size = .05) +
    theme_map() +
    labs(fill = 'LL', title = paste0("COVID-19 - Municípios RS - Intervalo de Credibilidade 2.5% - 
       Semana Epidemiológica ", unique(temp$time))) +
    theme(legend.position = "right") + 
    scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2]) + 
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
  
  gr <- list()
  gr <- list(LL = gLL, RR = gRR, UL = gUL)
  
}


BYM_SC = BYM2_semana_ep(17)
