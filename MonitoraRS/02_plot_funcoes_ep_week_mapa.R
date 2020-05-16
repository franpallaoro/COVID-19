library(ggplot2)
library(ggthemes)

#                             Semana Epidemiológica:
#---------------------------------------------------
#                       Estado do Rio Grande do Sul:
#---------------------------------------------------

options <- c('Casos Confirmados', 
             'Casos por\n 100.000 hab.', 'Óbitos', 'Letalidade', 'SIR')


rs_week <- function(input){
  
  temp <- dep_week %>%
    select(casos_cat, casos_p100_cat, mortes_cat, letalidade_cat, SIR_cat, municipio, geometry)
  
  temp <- temp[,c(which(options == input), 6:7)]
  names(temp)[1] <- 'variavel'
  
  if(which(options == input) == 1){
    virPallete <- viridis::viridis(n = 8)
  } else {
      virPallete <- viridis::viridis(n = 7)
  }
  
  ggplot() +
    geom_sf(data = subset(temp, subset = !is.na(variavel)),
            aes(fill = variavel), size = .05) +
    scale_fill_manual(values = virPallete) +
    theme_map() +
    labs(fill = paste0(options[which(options == input)]),
         caption = 'Fonte: Ministério da Saúde. Secretaria de Vigilância em Saúde (SVS)') +
    # facet_wrap( ~ time, ncol = 3) +
    theme(legend.position = "right")
  
}

#ggsave(filename = 'Casos.png', plot = rs_week(options[1]), 
#       path = 'C:/Users/Juliana/Downloads/series', width = 8, height = 8)

#ggsave(filename = 'Taxa.png', plot = rs_week(options[2]), 
#       path = 'C:/Users/Juliana/Downloads/series', width = 8, height = 8)

#ggsave(filename = 'SIR.png', plot = rs_week(options[3]), 
#       path = 'C:/Users/Juliana/Downloads/series', width = 8, height = 8)

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
  
  
  if(which(options == input) != 1){
    virPallete <- viridis::viridis(n = 7)
  } else {
    virPallete <- viridis::viridis(n = 8)
  }
  
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
         caption = 'Fonte: Secretarias de Saúde das Unidades Federativas, dados tratados por 
         Álvaro Justen e colaboradores/Brasil.IO') +
    theme(legend.position = "right") +
    facet_wrap( ~ time)
  
}

#ggsave(filename = 'Casos.png', plot = rs_week(options[1]), 
#       path = 'C:/Users/Juliana/Downloads/series', width = 8, height = 8)