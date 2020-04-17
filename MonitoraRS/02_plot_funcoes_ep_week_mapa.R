library(ggplot2)
library(ggthemes)
library(gganimate)

virPallete <- viridis::viridis(n = 7)

#                             Semana Epidemiológica:
#---------------------------------------------------
#                       Estado do Rio Grande do Sul:
#---------------------------------------------------

options <- c('Casos Confirmados', 'Óbitos', 
             'Casos por\n 100.000 hab.', 'Letalidade')


rs_week <- function(input){
  
  temp <- dep_week %>%
    select(casos_cat, mortes_cat, casos_p100_cat, 
           letalidade_cat, time, municipios, geometry)
  
  temp <- temp[,c(which(options == input), 5:7)]
  names(temp)[1] <- 'variavel'
  
  
  ggplot() +
    geom_sf(data = subset(temp, subset = !is.na(variavel)),
            aes(fill = variavel), size = .05) +
    scale_fill_manual(values = virPallete) +
    theme_map() +
    labs(fill = paste0(options[which(options == input)]),
         title = "COVID-19 - Municípios RS - 
         Acumulado por semana epidemiológica") +
    theme(legend.position = "right") +
    facet_wrap( ~ time)
}

rs_week(options[3])

#---------------------------------------------------
#                           Por Aglomerados Urbanos:
#---------------------------------------------------

agl_opt <- c('RMPA', 'Serra', 'Litoral Norte', 
             'Urbana Sul')

aglomerado_week <- function(input, input2){
  
  
  # definir a região 
  dep_aux <- dep_week %>%
    select(rmpa, serra, litoral_n, urba_sul, 
           casos_cat, mortes_cat, casos_p100_cat, letalidade_cat, 
           geometry, time)
  
  dep_aux <- dep_aux[,c(which(agl_opt == input2), 5:10)]
  names(dep_aux)[1] <- 'regiao'
  
  dep_aux <- dep_aux %>%
    filter(regiao == 1)
  
  
  # agora definir qual variavel mostrar
  
  temp <- dep_aux %>%
    select(casos_cat, mortes_cat, casos_p100_cat, 
           letalidade_cat, time, geometry)
  
  temp <- temp[,c(which(options == input), 5:6)]
  names(temp)[1] <- 'variavel'
  
  ggplot() +
    geom_sf(data = subset(temp, subset = !is.na(variavel)),
            aes(fill = variavel), size = .05) +
    scale_fill_manual(values = virPallete) +
    theme_map() +
    labs(fill = paste0(options[which(options == input)]),
         title = paste0('COVID-19 - Municípios ', 
                        agl_opt[which(agl_opt == input2)], 
                       " - Acumulado por semana epidemiológica")) +
    theme(legend.position = "right") +
    facet_wrap( ~ time)
  
}

aglomerado_week(input = options[3], input2 = agl_opt[3])
