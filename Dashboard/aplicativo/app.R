#-------------------------------------
# carregando os pacotes:

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(dplyr)
library(ggplot2)
library(here)
library(plotly)
library(ggiraph)
library(tidyr)
library(stringr)
library(forcats)
library(readxl)
library(tidyverse)
######## mapas
library(sp) #Caindo em desuso
library(sf) #Esta sendo a melhor opcao entre o sp
library(leaflet) #Talvez n usaremos
library(RColorBrewer)
library(DT)

################

theme_set(theme_gray())

options(OutDec= ",")

#-----------------------------------
# shapfiles dos estados
mapa_brasil <- sf::st_read("shapefiles/brasil_uf/BRUFE250GC_SIR.shp", quiet = TRUE) %>%
  mutate(NM_ESTADO = str_to_title(NM_ESTADO)) # todas os estados com letra  de título

# shapefile uf_municipios

lendo_shapefile_uf_mun <- function() {
  estados_siglas <- read_excel("estados_siglas.xlsx") %>%
    mutate(estado = str_to_title(Estado), id = as.factor(Sigla)) %>%
    arrange(Codigo)
  
  pasta <- "shapefiles/uf_municipios/"
  arquivos <- str_c(pasta,list.files(pasta, pattern = ".shp"))
  
  dados <- map(arquivos,sf::st_read, quiet = T)
  
  names(dados) <- estados_siglas$id
  
  return(dados)
  
}

dados_shp_uf_mun <- lendo_shapefile_uf_mun()

#transforma em um arquivo que o leaflet consegue ler!!!!
estados_siglas <- read_excel("estados_siglas.xlsx") %>%
  mutate(NM_ESTADO = str_to_title(Estado), id = as.factor(Sigla)) %>%
  select(NM_ESTADO, id)

labels_estado <- estados_siglas$id
names(labels_estado) <- estados_siglas$NM_ESTADO

mapa_brasil <- mapa_brasil %>% 
  merge(estados_siglas, by = 'NM_ESTADO') %>% 
  st_transform("+init=epsg:4326")

#-------------------------------------
# banco de dados de  casos confirmados:
covid <- readRDS(here::here('casos_covid19_br_mun.rds'))

# banco de dados por estado:
data_state <- covid %>%
  select(state, confirmed, deaths, confirmed_per_100k_inhabitants, 
         death_rate, is_last, place_type, estimated_population_2019) %>%
  filter(is_last == 'TRUE' & place_type == 'state')

# banco de dados obitos cartorio:

obitos_cartorio <- readRDS(here::here('obitos_br_uf.rds')) %>%
  filter(date >= "2020-03-16") # filtrando a partir do primeiro caso

names(obitos_cartorio) <- c("Estado","Data","Acumulado mortes COVID-19","Acumulado mortes Pneumonia 2019","Acumulado mortes Pneumonia 2020",
                            "Acumulado mortes por falha respiratória 2019","Acumulado mortes por falha respiratória 2020",
                            "Semana_epidemiologica_2019","Semana_epidemiologica_2020","Mortes COVID-19","Mortes Pneumonia 2019",
                            "Mortes Pneumonia 2020","Mortes por falha respiratória 2019","Mortes por falha respiratória 2020")


# banco de dados com total de casos no brasil por dia 2.0
# criei um novo para que não ficasse aqueles números negativos

pop_br = sum(data_state$estimated_population_2019)

ultima_atualizacao <- covid %>%
  select(state, confirmed, deaths, date, place_type, is_last) %>%
  filter(place_type == "state") %>%
  group_by(is_last) %>%
  filter(is_last) %>%
  summarise(confirmed = sum(confirmed), deaths = sum(deaths), conf_per100k = sum(confirmed)/pop_br*100000,
            letal = sum(deaths)/sum(confirmed)) %>%
  select(-is_last)

casos_br <- covid %>%
  select(state, confirmed, deaths, date, place_type, is_last) %>%
  filter(place_type == "state") %>%
  group_by(date) %>%
  summarise(confirmed = sum(confirmed), deaths = sum(deaths), conf_per100k = sum(confirmed)/pop_br*100000,
            letal = sum(deaths)/sum(confirmed)) %>%
  arrange(date)

casos_br[nrow(casos_br),c("confirmed","deaths","conf_per100k","letal")] <- ultima_atualizacao  

casos_br <- as.data.frame(casos_br[,c(2:5,1)])

# cor e ooções de seleção da variável a ser vista

fcolor <- c("#dd4b39", "#605ca8", "#f39c12", "#d81b60")
select_choices <- c("Casos Confirmados", "Óbitos", "Casos/100k hab.", "Letalidade")
select_choices2 <- c("confirmed","deaths","confirmed_per_100k_inhabitants","death_rate")

obts <- readRDS(here::here('obitos_br_uf.rds'))

temp <- obts %>%
  select(date, epidemiological_week_2020)

temp <- merge(casos_br, temp, by = 'date')
temp <- unique(temp)

casos_br <- casos_br %>%
  mutate(ep_week = temp$epidemiological_week_2020)

rm(temp)

# serie temporal com os dados no brasil
plot_geral <- function(input){
  
  temp <- as.data.frame(casos_br[,c(which(input == select_choices), 5)]) # dado selecionado no input
  temp <- data.frame(Data = as.character(stringr::str_sub(temp$date, 6, 10)), 
                     Frequencia = round(temp[,1], 3))
  
  col_sel <- fcolor[which(input == select_choices)]
  
  if(which(input == select_choices) < 3){
    
    Diario <- c(casos_br[1,which(input == select_choices)], 
                diff(casos_br[,which(input == select_choices)]))
    
    name_aux <- ifelse(input == select_choices[1], 
                       'Casos Acumulados', 'Óbitos Acumulados')
    
    p <- ggplot(temp) +
      geom_line(aes(x = Data, y = Frequencia, group = 1), color = col_sel, linetype = 'dotted') +
      geom_point(aes(x = Data, y = Frequencia), color = col_sel) + 
      geom_bar(aes(x = Data, y = Diario), fill = col_sel, stat = 'identity') + 
      theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5)) + 
      labs(x = NULL, y = name_aux) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(plot.background = element_rect(fill = "transparent", color = NA))
    
    
    ggplotly(p) %>%
      layout(xaxis = list(tickmode = 'array', 
                          tickvals = 1:nrow(temp), 
                          ticktext = unique(temp$Data))) 
    
  } else {
    
    p <- ggplot(temp) +
      geom_line(aes(x = Data, y = Frequencia, group = 1), color = col_sel) +
      theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5)) + 
      labs(x = NULL, y = paste0(input)) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      theme(plot.background = element_rect(fill = "transparent", color = NA))
    
    
    ggplotly(p) %>%
      layout(xaxis = list(tickmode = 'array', 
                          tickvals = 1:nrow(temp), 
                          ticktext = unique(temp$Data)))
  }
}

# mapa dos dados por estado
plot_mapa <- function(input){
  
  dataset <- data_state %>%
    select(confirmed, deaths, confirmed_per_100k_inhabitants, death_rate, state) %>%
    mutate(id = state) 
  
  dataset <- dataset %>%
    mutate(variavel = dataset[,which(input == select_choices)], id = as.factor(id)) %>%
    select(id, variavel) 
  #########################################################################################
  #### MAPA  
  #########################################################################################
  
  paleta <- c("Reds","Purples","Oranges","PuRd")[which(input == select_choices)]
  
  tidy <- merge(mapa_brasil, dataset, by.x = "id", by.y = "id")
  tidy = st_as_sf(tidy)
  tidy <- st_transform(tidy, "+init=epsg:4326") ##leaflet
  
  pal = colorQuantile(palette=paleta, domain =tidy$variavel, n = 5)
  #fcolor <- c("#dd4b39", "#605ca8", "#f39c12", "#d81b60")
  #selcor = fcolor[1]
  #pal <- colorBin("RdYlBu", domain =c(0, max(tidy$variavel), bins = 8), reverse= TRUE) ## cor da legenda
  
  leaflet(tidy) %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
    addPolygons(fillColor = ~pal(variavel), 
                weight = 1.5,
                opacity = 0.7,
                fillOpacity = 0.7,
                color = "gray",
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = sprintf("%s - %s", tidy$NM_ESTADO, tidy$variavel),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "6px 11px"),
                  textsize = "15px",
                  direction = "auto"))   %>%
    addLegend(pal = pal, values = round(tidy$variavel,4), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
      n = length(cuts)
      paste0(round(cuts[-n],2), " &ndash; ", round(cuts[-1],2))},
      title = select_choices[which(input == select_choices)],
      labels = ~tidy$NM_ESTADO,
      position = "bottomright")
  
  #addLegend(pal = pal, values = ~tidy$variavel, opacity = 0.7, # legenda para colorBin
  #          title = select_choices[which(input == select_choices)],
  #labels = ~tidy$NM_ESTADO,
  #          position = "bottomright")
  
  #########################################################################################
  #########################################################################################
  
}

# gráfico de barras por estado
plot_bar <- function(input){
  
  Frequencia = round(as.vector(data_state[,(which(input == select_choices) + 1)]), 2)
  selcor = fcolor[which(input == select_choices)]
  
  data_state$Frequencia <- Frequencia   ### Para organizar o grafico por frequencia do estado
  data_state = data_state %>%
    arrange(Frequencia)
  ordem <- data_state$state
  
  temp <- data_state %>%
    select(state, Frequencia) %>% 
    mutate(obs_text = Frequencia)
  
  names(temp)[1] <- 'UF'
  
  
  p = ggplot(temp, aes(x = UF, y = Frequencia)) +
    geom_col(fill = selcor) +
    geom_text(aes(label = paste0(temp$obs_text)), size = 3) +
    scale_x_discrete(limits = ordem) +
    coord_flip() +
    ylim(0, max(Frequencia) + mean(Frequencia)) + 
    labs(x = NULL, y = NULL) + 
    theme(plot.background = element_rect(fill = "transparent", color = NA), 
          axis.ticks.x = element_blank(), axis.text.x = element_blank())
  
  
  ggplotly(p, tooltip = c('UF', 'Frequencia')) %>%
    style(textposition = "middleright") %>%
    layout(yaxis = list(tickmode = 'array', 
                        tickvals = 1:nrow(temp), 
                        ticktext = unique(temp$UF)))
}

# dados do brasil por semana epi
week_geral <- function(input){
  
  # banco com última atualização para não ficar dados negativos
  
  ultima_atualizacao <- covid %>%
    select(state, confirmed, deaths, date, place_type, is_last) %>%
    filter(place_type == "state") %>%
    group_by(is_last) %>%
    filter(is_last) %>%
    summarise(confirmed = sum(confirmed), deaths = sum(deaths), conf_per100k = sum(confirmed)/pop_br*100000,
              letal = sum(deaths)/sum(confirmed))
  
  #-- banco de dados para semana epidemiologica
  temp <-  covid %>%
    select(state, confirmed, deaths, 
           date, place_type) %>%
    filter(place_type == 'state') %>%
    aggregate(cbind(confirmed, deaths) ~ date, data = ., sum)
  
  temp[nrow(temp),c("confirmed","deaths")] <- c(ultima_atualizacao$confirmed,ultima_atualizacao$deaths)
  
  temp2 <- obts %>%
    select(date, epidemiological_week_2020)
  
  temp2 <- merge(temp, temp2, by = 'date') %>%
    unique()
  
  temp2 <- temp2 %>%
    mutate(conf_diario = c(confirmed[1], diff(confirmed))) %>%
    mutate(deaths_diario = c(deaths[1], diff(deaths))) %>%
    aggregate(cbind(conf_diario, deaths_diario) ~ epidemiological_week_2020, data = ., sum) %>%
    mutate(taxa = conf_diario/pop_br*100000) %>%
    mutate(letal = deaths_diario/conf_diario)
  
  colnames(temp2) <- c('ep_week', 'confirmed', 'deaths', 'taxa_per_100k', 'letal')
  
  temp2$taxa_per_100k <- round(temp2$taxa_per_100k, 3)
  temp2$letal <- round(temp2$letal, 3)
  
  
  temp2 <- temp2 %>%
    select(confirmed, deaths, taxa_per_100k, letal, ep_week)
  
  temp <- temp2[,c(which(input == select_choices), 5)] # dado selecionado no input
  col_sel <- fcolor[which(input == select_choices)]
  colnames(temp)[1] <- 'Frequencia'
  
  if(which(input == select_choices) < 3){
    
    name_aux <- ifelse(input == select_choices[1], 
                       'Casos Acumulados', 'Óbitos Acumulados')
    
    temp$Acumulado <- cumsum(temp$Frequencia)
    
    p <- ggplot(temp) +
      geom_line(aes(x = ep_week, y = Acumulado, group = 1), color = col_sel, linetype = 'dotted') +
      geom_point(aes(x = ep_week, y = Acumulado), color = col_sel) + 
      geom_bar(aes(x = ep_week, y = Frequencia), fill = col_sel, stat = 'identity') + 
      theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5)) + 
      labs(x = NULL, y = name_aux) + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      theme(plot.background = element_rect(fill = "transparent", color = NA)) 
    
    ggplotly(p) %>%
      layout(xaxis = list(tickmode = 'array', 
                          tickvals = temp$ep_week, 
                          ticktext = temp$ep_week))
    
  } else {
    
    p <- ggplot(temp) +
      geom_line(aes(x = ep_week, y = Frequencia, group = 1), color = col_sel) +
      geom_point(aes(x = ep_week, y = Frequencia, group = 1), color = col_sel) +
      theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5)) + 
      labs(x = NULL, y = paste0(input)) + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
      theme(plot.background = element_rect(fill = "transparent", color = NA))
    
    ggplotly(p) %>%
      layout(xaxis = list(tickmode = 'array', 
                          tickvals = temp$ep_week, 
                          ticktext = temp$ep_week))
    
  }
}

# funtion mapa uf

plot_mapa_uf <- function(estado,input) {
  
  aux_var <- select_choices2[which(input == select_choices)]
  
  var <- rlang::sym(aux_var)
  
  aux <- covid %>%
    filter(is_last & place_type=="city" & state == estado) %>%
    select(city_ibge_code,!!var)
  
  dados_mapa <- dados_shp_uf_mun[[estado]] %>%
    mutate(municipio = str_to_title(NM_MUNICIP)) %>%
    left_join(aux, by = c("CD_GEOCMU" = "city_ibge_code")) %>%
    mutate(var = !!var)
  
  y_quantidade <- dados_mapa$var
  
  #y_quantidade <- replace_na(y_quantidade, 0) # não rodar pois fica pior ainda o problema do range dos bins
  
  if (aux_var == "confirmed") {
    paleta <- "Reds"
    arredonda <- 0
  } else if (aux_var == "deaths") {
    paleta <- "Purples"
    arredonda <- 0
  } else if (aux_var == "confirmed_per_100k_inhabitants") {
    paleta <- "Oranges"
    arredonda <- 2
  } else {
    paleta <- "PuRd"
    arredonda <- 2
    dados_mapa$var <- 100*dados_mapa$var
  }
  
  pal <- colorQuantile(palette=paleta, domain = y_quantidade, n = 5, na.color = "#fff5f0")
  
  leaflet(dados_mapa) %>%
    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
    addPolygons(fillColor = ~pal(y_quantidade), 
                weight = 1.5,
                opacity = 0.7,
                fillOpacity = 0.7,
                color = "gray",
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = sprintf("%s - %s", dados_mapa$municipio, y_quantidade),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "6px 11px"),
                  textsize = "15px",
                  direction = "auto"))   %>%
    addLegend(pal = pal, values = round(y_quantidade,arredonda), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
      n = length(cuts)
      paste0(round(cuts[-n],arredonda), " &ndash; ", round(cuts[-1],arredonda))},
      title = select_choices[which(estado == select_choices)],
      labels = ~dados_mapa$municipio,
      position = "bottomright")
  
}

# function tabela tabItem UF

tabela_uf <- function(estado, input) {
  
  aux_var <- select_choices2[which(input == select_choices)]
  
  var <- rlang::sym(aux_var)
  
  aux <- covid %>%
    filter(is_last & place_type=="city" & state == estado) %>%
    select(city,!!var) %>%
    arrange(desc(!!var))
  
  texto <- ifelse(
    test = aux_var == "confirmed",
    yes = "Confirmados",
    no = ifelse(
      test = aux_var == "deaths",
      yes = "Óbitos",
      no = ifelse(
        test = aux_var == "confirmed_per_100k_inhabitants",
        yes = "Confirmados/100mil hab.",
        no = "Letalidade"
      )
    ))
  
  tabela <- datatable(
    aux[,c("city",aux_var)], 
    rownames=F,
    class = "compact",
    colnames = c("Município",texto),
    options = list(
      dom = "tS", 
      ordering = F,
      scrollY = "560px",
      paging = FALSE
    )
  ) %>%
    formatStyle("city",color = "#787878", fontSize = "14px", backgroundColor = "#f0f0f0") %>%
    formatStyle(aux_var, color = fcolor[which(input==select_choices)], fontWeight = "bold",fontSize = "14px", backgroundColor = "#f0f0f0")
  
  if(aux_var %in% c("confirmed_per_100k_inhabitants","death_rate")) {
    tabela <- formatRound(tabela, aux_var, digits = 2)
  }
  
  tabela
  
}

# function série uf 

plot_serie_uf <- function(estado, input, tipo) {
  
  aux_var <- select_choices2[which(input == select_choices)]
  
  var <- rlang::sym(aux_var)
  
  col_sel <- fcolor[which(input == select_choices)]
  
  aux <- covid %>%
    filter(place_type=="state" & state == estado) %>%
    arrange(date)
  
  if(tipo == "Diário") {
    
    ordem <- as.character(format(aux$date, "%d-%m"))
    
    if(aux_var %in% select_choices2[c(1,2)]) {
      
      aux$novos <- c(aux[1,aux_var],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- aux[i,aux_var]-aux[i-1,aux_var]
      }
      
      aux$date <- as.character(format(aux$date, "%d-%m"))
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = !!var, group = 1), color = col_sel, linetype = 'dotted') +
        geom_point(aes(x = date, y = !!var), color = col_sel) + 
        geom_col(aes(x = date, y = novos), fill = col_sel) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = input) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
    } else {
      
      aux$date <- as.character(format(aux$date, "%d-%m"))
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = !!var, group = 1), color = col_sel, linetype = 'dotted') +
        geom_point(aes(x = date, y = !!var), color = col_sel) + 
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = input) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
    }
    
    
    
  } else {
    
    temp2 <- obts %>%
      select(date, epidemiological_week_2020) %>%
      filter(date %in% aux$date) %>%
      unique()
    
    aux <- left_join(aux,temp2, by = "date") %>%
      group_by(epidemiological_week_2020) %>%
      filter(date == max(date)) %>%
      ungroup()
    
    ordem <- as.character(aux$epidemiological_week_2020)
    
    
    
    if(aux_var %in% select_choices2[c(1,2)]) {
      
      aux$novos <- c(as.data.frame(aux)[1,aux_var],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- as.data.frame(aux)[i,aux_var]-as.data.frame(aux)[i-1,aux_var]
      }
      
      aux$epidemiological_week_2020 <- as.character(aux$epidemiological_week_2020)
      
      p <- ggplot(aux) +
        geom_line(aes(x = epidemiological_week_2020, y = !!var, group = 1), color = col_sel, linetype = 'dotted') +
        geom_point(aes(x = epidemiological_week_2020, y = !!var), color = col_sel) + 
        geom_col(aes(x = epidemiological_week_2020, y = novos), fill = col_sel) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Semana Epidemiológica", y = input) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
    } else {
      
      aux$epidemiological_week_2020 <- as.character(aux$epidemiological_week_2020)
      
      p <- ggplot(aux) +
        geom_line(aes(x = epidemiological_week_2020, y = !!var, group = 1), color = col_sel, linetype = 'dotted') +
        geom_point(aes(x = epidemiological_week_2020, y = !!var), color = col_sel) + 
        scale_x_discrete(limits = ordem) +
        labs(x = "Semana Epidemiológica", y = input) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
    }
    
  }
  
  ggplotly(p) 
  
}

# function plot óbitos do cartório
plot_cart <- function(input) {
  
  if(input == "Diário") {
    var <- rlang::sym("Data")
    text <- "Dia desde o primeiro óbito COVID-19"
    text2 <- "Número de óbitos diários"
  } else {
    var <- rlang::sym("Semana_epidemiologica_2020")
    text <- "Semana epidemiológica desde o primeiro óbito COVID-19"
    text2 <- "Número de óbitos por semana epidemiólogica"
  }
  
  aux <- obitos_cartorio %>%
    group_by(!!var) %>%
    summarise_at(names(obitos_cartorio)[c(3:7,10:14)],sum) %>%
    pivot_longer(
      cols = -c(!!var),
      names_to = "disease_type",
      values_to = "deaths"
    ) %>%
    filter(!str_detect(disease_type,"^Acumulado"))
  
  valores <- c("Mortes Pneumonia 2019","Mortes Pneumonia 2020","Mortes por falha respiratória 2019",
               "Mortes por falha respiratória 2020","Mortes COVID-19")
  
  paleta <- RColorBrewer::brewer.pal("Paired", n = 5)
  names(paleta) <- valores
  
  if(input == "Diário") {
    
    aux$Data <- as.character(stringr::str_sub(aux$Data, 6, 10))
    
    p1 <- ggplot(aux) +
      geom_line(aes(x = !!var, y = deaths, color = disease_type, group = 1), linetype = 'dotted') +
      geom_point(aes(x = !!var, y = deaths, color = disease_type)) + 
      scale_color_manual(name = "Doenças", values = paleta) +
      labs(x = text, y = text2) + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p1) %>%
      layout(xaxis = list(tickmode = 'array', 
                          tickvals = 1:length(unique(aux$Data)), 
                          ticktext = unique(aux$Data)))
    
  } else {
    
    p1 <- ggplot(aux) +
      geom_line(aes(x = !!var, y = deaths, color = disease_type, group = 1), linetype = 'dotted') +
      geom_point(aes(x = !!var, y = deaths, color = disease_type)) + 
      scale_color_manual(name = "Doenças", values = paleta) +
      labs(x = text, y = text2) + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p1) %>%
      style(textposition = "middleright") 
  }
  
}

# óbitos do cartório separados por tipo 
obitos_separados <- function(input, escolha){
  
  if(input == "Diário") {
    
    var <- rlang::sym("Data")
    text <- "Dia desde o primeiro óbito COVID-19"
    text2 <- "Número de óbitos diários"
    
  } else {
    
    var <- rlang::sym("Semana_epidemiologica_2020")
    text <- "Semana epidemiológica desde o primeiro óbito COVID-19"
    text2 <- "Número de óbitos por semana epidemiólogica"
    
  }
  
  aux <- obitos_cartorio %>%
    group_by(!!var) %>%
    summarise_at(names(obitos_cartorio)[c(3:7,10:14)],sum) %>%
    pivot_longer(
      cols = -c(!!var),
      names_to = "disease_type",
      values_to = "deaths"
    ) %>%
    filter(!str_detect(disease_type,"^Acumulado"))
  
  valores <- c("Mortes Pneumonia 2019","Mortes Pneumonia 2020","Mortes por falha respiratória 2019",
               "Mortes por falha respiratória 2020","Mortes COVID-19")
  
  paleta <- RColorBrewer::brewer.pal("Paired", n = 5)
  names(paleta) <- valores
  
  
  if (escolha == 1) {
    
    aux <- aux %>% 
      filter(disease_type == 'Mortes Pneumonia 2019' | disease_type == 'Mortes Pneumonia 2020' | 
               disease_type == 'Mortes COVID-19')
    
  } else {
    
    aux <- aux %>% 
      filter(disease_type == 'Mortes por falha respiratória 2019' | disease_type == 'Mortes COVID-19' |
               disease_type == 'Mortes por falha respiratória 2020' )
    
  }
  
  
  if(input == "Diário") {
    
    aux$Data <- as.character(stringr::str_sub(aux$Data, 6, 10))
    
    p1 <- ggplot(aux) +
      geom_line(aes(x = !!var, y = deaths, color = disease_type, group = 1), linetype = 'dotted') +
      geom_point(aes(x = !!var, y = deaths, color = disease_type)) + 
      scale_color_manual(name = "Doenças", values = paleta) +
      labs(x = text, y = text2) + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    ggplotly(p1) %>%
      layout(xaxis = list(tickmode = 'array', 
                          tickvals = 1:length(unique(aux$Data)), 
                          ticktext = unique(aux$Data)), 
             legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5))
  } else {
    
    p1 <- ggplot(aux) +
      geom_line(aes(x = !!var, y = deaths, color = disease_type, group = 1), linetype = 'dotted') +
      geom_point(aes(x = !!var, y = deaths, color = disease_type)) + 
      scale_color_manual(name = "Doenças", values = paleta) +
      labs(x = text, y = text2) + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    ggplotly(p1) %>%
      style(textposition = "middleright") %>%
      layout(legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5))
  }
}


#-------------------------------------
# ui do dashboard:

ui <- dashboardPage(
  #-------------------------------------
  # Titulo do dashboard:
  
  dashboardHeader(
    title = shinyDashboardLogoDIY(
      boldText = "COVID"
      ,mainText = "-19"
      ,textSize = 16
      ,badgeText = "CovidMetrika"
      ,badgeTextColor = "white"
      ,badgeTextSize = 2
      ,badgeBackColor = "#39cccc"
      ,badgeBorderRadius = 3)
  ),
  
  #-------------------------------------
  # Menu do dashboard:
  
  dashboardSidebar(
    sidebarMenu(
      selectInput(inputId = 'typevar', label = "Selecione a variável:", 
                  choices = select_choices, selected = select_choices[1]),
      
      menuItem("Covid-19", icon = icon("chart-area"), tabName = "dashbr", 
               badgeLabel = "BR", badgeColor = "teal"),
      menuItem("Covid-19", icon = icon("chart-bar"), tabName = "dashuf", 
               badgeLabel = "UF", badgeColor = "teal"),
      menuItem("Total de Óbitos", icon = icon("chart-line"), tabName = "cart", badgeLabel = "BR", badgeColor = "teal"),
      menuItem("Fonte de Dados", icon = icon("file-download"), tabName = "dados", badgeColor = "teal"),
      menuItem("CovidMetrika", icon = icon("users"), tabName = "us", badgeColor = "teal"), 
      menuItem("Source Code", icon = icon("code"), badgeColor = "teal", 
               href = 'https://github.com/franpallaoro/COVID-19/tree/master/Dashboard')
    )
  ),
  
  #-------------------------------------
  # 'corpo' do dashboard: 
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      tabItem("dashbr", 
              
              fluidRow(
                #-------------------------------------
                # as três 'caixas' com informações resumo: 
                
                valueBoxOutput("casosBox", width = 3),
                valueBoxOutput("obitosBox", width = 3),
                valueBoxOutput("taxaBox", width = 3),
                valueBoxOutput("letalBox", width = 3),
                
                #-------------------------------------
                # plot geral - primeiro plot
                box(leafletOutput("mapaPlot", height = 350L), width = 6L, height = 390L),
                box(plotlyOutput("barPlot", height = 350L), width = 6L, height = 390L),   
                column(width = 12,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Diário", 
                                            box(width = NULL, 
                                                plotlyOutput("confPlot", height = 300L))
                                   ), #tabpanel 1
                                   
                                   tabPanel("Semana Epidemiológica", 
                                            box(width = NULL, 
                                                plotlyOutput("conf2Plot", height = 300L))
                                   ) #tabpanel 2
                                   
                       ) #tabset
                       
                ) # coluna 
                
                
              ) #fluidrow
      ), # final da parte dos dados nacionais
      
      tabItem("dashuf",
              
              selectInput(
                "estado",
                label = "Escolha um estado",
                choices = labels_estado,
                selected = "RS"
              ),
              
              fluidRow(
                valueBoxOutput("casos_uf", width = 3),
                valueBoxOutput("obitos_uf", width = 3),
                valueBoxOutput("taxa_uf", width = 3),
                valueBoxOutput("letal_uf", width = 3)
              ),
              fluidRow(
                box(
                  leafletOutput("mapa_uf", height = 390L),
                  width = 9,
                  height = 410L
                ),
                box(
                  dataTableOutput("table_uf", height = "390px"),
                  width = 3,
                  height = 410L
                ),
                column(
                  width = 12,
                  tabBox(id = "tab_uf",
                         width = 12,
                         title = NULL,
                         tabPanel("Diário",
                                  plotlyOutput("uf_dia_plot", height = 350)
                         ),
                         tabPanel("Semana Epidemiológica",
                                  plotlyOutput("uf_sem_plot", height = 350)
                         )
                         
                  )
                )
              )
              
      ),
      tabItem("cart",
              
              # pegando de alguma forma htmlzada a versão mais recente dos ícones font awesome
              
              tags$script(src = "https://kit.fontawesome.com/ab15947b75.js", crossorigin="anonymous"), 
              
              # reescrevendo as cores default para as que eu quero nas boxes de óbitos cartório
              
              tags$style(".small-box.bg-navy { background-color: #1F78B4 !important; color: #FFFFFF !important; }"),
              tags$style(".small-box.bg-olive { background-color: #33A02C !important; color: #FFFFFF !important; }"),
              tags$style(".small-box.bg-lime { background-color: #FB9A99 !important; color: #FFFFFF !important; }"),
              
              fluidRow(
                valueBoxOutput("box_pneumonia", width = 4),
                valueBoxOutput("box_falha", width = 4),
                valueBoxOutput("box_covid", width = 4),
                column(
                  width = 12,
                  tabBox(id = "tab_cart",
                         width = 12,
                         title = "Número de óbitos novos registrados em cartório",
                         tabPanel("Diário",
                                  plotlyOutput("cart_dia_plot", height = 600)
                         ),
                         tabPanel("Semana Epidemiológica",
                                  plotlyOutput("cart_sem_plot", height = 600)
                         )
                         
                  ) # tabBox 
                  
                ) # coluna
              ) # fluid
              
      ),
      tabItem("dados", 
              
              fluidRow(
                infoBox(tags$p("Brasil.io", style = "font-size: 200%;"), 
                        subtitle = 'Fonte: Secretarias de Saúde das Unidades Federativas, 
                        dados tratados por Álvaro Justen e colaboradores',
                        icon = icon("at"), color = "teal", width = 12, 
                        href = "https://brasil.io/dataset/covid19/caso", fill = TRUE),
                
                valueBox('Sobre:', subtitle = 'Informações dos dados', icon = icon("info"), 
                         color = 'teal', width = 3, 
                         href = 'https://github.com/turicas/covid19-br/blob/master/api.md#casos'),
                
                valueBox('Dados:', subtitle = 'Download', icon = icon("download"), 
                         color = 'teal', width = 3,
                         href = 'https://data.brasil.io/dataset/covid19/_meta/list.html'),
                
                valueBox('F.A.Q.:', subtitle = 'Perguntas Frequentes', icon = icon("question-circle"), 
                         color = 'teal', width = 3,
                         href = 'https://github.com/turicas/covid19-br/blob/master/faq.md'),
                
                valueBox('Licença:', subtitle = '(CC BY-SA 4.0)', icon = icon("creative-commons"), 
                         color = 'teal', width = 3,
                         href = 'https://creativecommons.org/licenses/by-sa/4.0/deed.en')
                
              ) #fluidRow
              
              
              
      ), 
      tabItem("us",
              
              widgetUserBox(
                title = tags$b("Franciele Lobo Pallaoro"),
                subtitle = "Estudante de Estatística da UFRGS",
                type = 2,
                width = 4,
                src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/franciele.jpg?raw=true',
                color = "light-blue",
                footer = "Contato: franpallaoro@gmail.com"
              ),
              
              widgetUserBox(
                title = tags$b("Gabriel Holmer Saul"),
                subtitle = "Estudante de Estatística da UFRGS",
                type = 2,
                width = 4,
                src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/gabriel.jpg?raw=true',
                color = "teal",
                footer = "Contato: gabrielholmersaul@gmail.com"
              )
              ,
              
              widgetUserBox(
                title = tags$b("Gustavo Machado Utpott"),
                subtitle = "Estudante de Estatística da UFRGS",
                type = 2,
                width = 4,
                src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/gustavo.png?raw=true',
                color = "light-blue",
                footer = "Contato: gustavo.utpott@gmail.com"
              ),
              
              widgetUserBox(
                title = tags$b("Juliana Sena de Souza"),
                subtitle = "Estudante de Pós-Graduação em Epidemiologia da UFRGS",
                type = 2,
                width = 4,
                src =  'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/juliana.jpeg?raw=true',
                color = "teal",
                footer = "Contato: julianass.estatistica@gmail.com"
              ),
              
              
              widgetUserBox(
                title = tags$b("Márcia Helena Barbian"),
                subtitle = "Professora do departamento de Estatística da UFRGS",
                type = 2,
                width = 4,
                src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/marcia.png?raw=true',
                color = "light-blue",
                footer =  "Contato: mhbarbian@gmail.com"
              ), 
              
              widgetUserBox(
                title = tags$b("Rodrigo Citton P. dos Reis"),
                subtitle = "Professor do departamento de Estatística da UFRGS",
                type = 2,
                width = 4,
                src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/rodrigo.jpg?raw=true',
                color = "teal",
                footer =  "Contato: citton.padilha@ufrgs.br"
              ), 
              
              tags$img(src = "https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/logos.png?raw=true", 
                       height = "150", width = "1000")
              
              
      ) 
      )
    
  ) # dashboard body
  ) # final de ui
#-------------------------------------


#-------------------------------------
server <- function(input, output) {
  #-------------------------------------
  #-------------------------------------
  
  # tabItem dashbr
  
  # 'output' das caixas de informações principais: 
  
  output$casosBox <- renderValueBox({
    valueBox(casos_br[nrow(casos_br),"confirmed"], "Casos", icon = icon("ambulance"),
             color = "red"
    )
  })
  
  output$obitosBox <- renderValueBox({
    valueBox(casos_br[nrow(casos_br),"deaths"], "Óbitos", icon = icon("skull"),
             color = "purple"
    )
  })
  
  output$taxaBox <- renderValueBox({
    valueBox(round(casos_br[nrow(casos_br),"conf_per100k"],2), "Casos /100 mil hab.", icon = icon("heartbeat"),
             color = "yellow"
    )
  })
  
  output$letalBox <- renderValueBox({
    valueBox(
      paste0(round(casos_br[nrow(casos_br),"letal"]*100, 2), '%'), 
      "Letalidade", icon = icon("exclamation-circle"),
      color = "maroon"
    )
  })
  #-------------------------------------
  
  # gráfico com o número de casos geral 
  output$confPlot <- renderPlotly({
    plot_geral(input$typevar)
  })
  
  output$conf2Plot <- renderPlotly({
    week_geral(input$typevar)
  })
  
  # grafico com o mapa de casos por UF
  output$mapaPlot <- renderLeaflet({
    plot_mapa(input$typevar)
  })
  
  output$barPlot <- renderPlotly({
    plot_bar(input$typevar)
  })
  
  #-------------------------------------
  #-------------------------------------
  
  # tabItem dashuf
  
  # boxes
  
  output$casos_uf <- renderValueBox({
    aux <- covid %>%
      filter(state == input$estado) %>%
      filter(is_last) %>%
      filter(place_type == "state")
    
    valueBox(aux$confirmed, "Casos", icon = icon("ambulance"),
             color = "red"
    )
  })
  
  output$obitos_uf <- renderValueBox({
    aux <- covid %>%
      filter(state == input$estado) %>%
      filter(is_last) %>%
      filter(place_type == "state")
    
    valueBox(aux$deaths, "Óbitos", icon = icon("skull"),
             color = "purple"
    )
  })
  
  output$taxa_uf <- renderValueBox({
    aux <- covid %>%
      filter(state == input$estado) %>%
      filter(is_last) %>%
      filter(place_type == "state")
    
    valueBox(round(aux$confirmed_per_100k_inhabitants,2), "Taxa /100k hab.", icon = icon("heartbeat"),
             color = "yellow"
    )
  })
  
  output$letal_uf <- renderValueBox({
    aux <- covid %>%
      filter(state == input$estado) %>%
      filter(is_last) %>%
      filter(place_type == "state")
    
    valueBox(
      paste0(round(aux$death_rate*100, 2), '%'), 
      "Letalidade", icon = icon("exclamation-circle"),
      color = "maroon"
    )
  })
  
  # mapa_uf 
  
  output$mapa_uf <- renderLeaflet({
    
    plot_mapa_uf(estado = input$estado, input = input$typevar)
    
  })
  
  # tabela_uf
  
  output$table_uf <- renderDataTable({
    
    tabela_uf(estado = input$estado, input = input$typevar)
    
  })
  
  # série_uf diário
  
  output$uf_dia_plot <- renderPlotly({
    
    plot_serie_uf(estado = input$estado, input = input$typevar, tipo = "Diário")
    
  })
  
  # série_uf semanal
  
  output$uf_sem_plot <- renderPlotly({
    
    plot_serie_uf(estado = input$estado, input = input$typevar, tipo = "Semana Epidemiológica")
    
  })
  
  #-------------------------------------
  #-------------------------------------
  
  # tabItem obitos cartorio
  
  # boxes 
  
  output$box_pneumonia <- renderValueBox({
    valueBox(
      value = sum(obitos_cartorio$`Mortes Pneumonia 2020`), 
      subtitle = "Pneumonia (desde o 1º óbito Covid-19)", 
      icon = tags$i(class = "fa fa-lungs"),
      color = "navy"
    )
  })
  
  output$box_falha <- renderValueBox({
    valueBox(
      value = sum(obitos_cartorio$`Mortes por falha respiratória 2020`), 
      subtitle = "Falha Respiratória (desde o 1º óbito Covid-19)", 
      icon = tags$i(class = "fa fa-lungs"),
      color = "olive"
    )
  })
  
  output$box_covid <- renderValueBox({
    valueBox(
      value = sum(obitos_cartorio$`Mortes COVID-19`), 
      subtitle = "Covid-19", 
      icon = tags$i(class = "fa fa-virus"),
      color = "lime"
    )
  })
  
  # gráfico com óbitos por dia
  
  output$cart_dia_plot <- renderPlotly({
    plot_cart(input$tab_cart)
  })
  
  # gráfico com óbitos por semana epidemiologica
  
  output$cart_sem_plot <- renderPlotly({
    plot_cart(input$tab_cart)
  })
  #-------------------------------------
  
  
}

shinyApp(ui = ui, server = server)