library(shiny)
library(tidyverse)
library(sf)
library(stringr)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(DT)
library(leafpop)
library(readxl)
library(rgeos) #https://statistique-et-logiciel-r.com/premiers-pas-en-cartographie-avec-r/
library(viridis)
library(rainbow)
library(httr)
library(curl)
library(abjutils)
library(shinydashboard)
library(plotly)
library(scales)
library(shinydashboardPlus)
library(shinyEffects)

####
# Leitura banco de dados
####

# rodando o script de data_wrangling

source("data_wrangling.R", encoding = "UTF-8")

####
# Funções criadas
####

# criando função personalizada para a caixa do usuário 

widgetUserBoxx <- function (..., title = NULL, subtitle = NULL, type = NULL, background = FALSE, 
                            backgroundUrl = NULL, src = NULL, color = NULL, footer = NULL, 
                            footer_padding = TRUE, width = 6, height = NULL, boxToolSize = "sm", 
                            collapsible = TRUE, closable = FALSE) 
{
  cl <- "widget-user-header"
  if (!is.null(color) && background == FALSE) 
    cl <- paste0(cl, " bg-", color)
  if (isTRUE(background)) 
    cl <- paste0(cl, " bg-black")
  boxCl <- "box box-widget widget-user"
  if (!is.null(type)) 
    boxCl <- paste0(boxCl, "-", type)
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  backgroundStyle <- NULL
  if (isTRUE(background)) {
    backgroundStyle <- paste0("background: url('", 
                              backgroundUrl, "') center center;")
  }
  shiny::column(width = width, shiny::tags$div(class = boxCl, 
                                               style = style, shiny::tags$div(class = cl, style = backgroundStyle, 
                                                                              shiny::tags$div(class = "pull-right box-tools", 
                                                                                              if (collapsible) {
                                                                                                shiny::tags$button(class = paste0("btn", 
                                                                                                                                  " bg-", color, " btn-", boxToolSize), 
                                                                                                                   `data-widget` = "collapse", type = "button", 
                                                                                                                   shiny::tags$i(class = "fa fa-minus"))
                                                                                              }, if (closable) {
                                                                                                shiny::tags$button(class = paste0("btn", 
                                                                                                                                  " bg-", color, " btn-", boxToolSize), 
                                                                                                                   `data-widget` = "remove", type = "button", 
                                                                                                                   shiny::tags$i(class = "fa fa-times"))
                                                                                              }), 
                                                                              shiny::tags$h3(class = "widget-user-username",
                                                                                             title), shiny::tags$h5(class = "widget-user-desc",
                                                                                                                    subtitle)), shiny::tags$div(class = "box-body", 
                                                                                                                                                ...), shiny::tags$div(class = if (isTRUE(footer_padding)) 
                                                                                                                                                  "box-footer"
                                                                                                                                                  else "box-footer no-padding", footer)))
}

##############################################################################################
# Aplicativo
##############################################################################################


header <- dashboardHeader(
  title = "Dados COVID19 no Rio Grande do Sul",
  titleWidth = 500
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dados COVID-19 RS", tabName = "mapa_covid_rs"),
    menuItem("Leitos UTI - Adulto RS", tabName = "mapa_leitos_rs"),
    menuItem("Fonte de dados", tabName = "fonte"),
    menuItem("CovidMetrika", tabName = "sobre")
  ),
  width = 180
)


body <- dashboardBody(
  tabItems(
    tabItem("mapa_covid_rs",
            fluidPage(
              titlePanel(
                h1("Mapa com dados do COVID-19 Rio Grande do Sul")
              ),
              
              # reescrevendo as cores default para as que eu quero nas boxes de óbitos cartório
              tags$style(".small-box.bg-lime { background-color: #757474 !important; color: #FFFFFF !important; }"),
              
              fluidRow(
                valueBoxOutput("box_conf", width = 3),
                valueBoxOutput("box_inci", width = 3),
                valueBoxOutput("box_obit", width = 3),
                valueBoxOutput("box_leta", width = 3)
              ),
              fluidRow(
                column(
                  width = 9,
                  fluidRow(
                    column(
                      width = 6,
                      h3("Selecione a variável de interesse"),
                      radioButtons("var_covid",
                                   label = NULL,
                                   choices = list("Confirmados" = "confirmed","Confirmados por 100mil hab." = "confirmed_per_100k_inhabitants","Óbitos" = "deaths","Letalidade" = "death_rate"),
                                   selected = "confirmed",
                                   inline = T)
                    ),
                    column(
                      width = 6,
                      h3("Selecione o tipo de agrupamento"),
                      radioButtons("var_covid_2",
                                   label = NULL,
                                   choices = list("Municípios" = "municipio", "Mesoregiões" = "meso_regiao"),
                                   selected = "municipio",
                                   inline = T),
                    )
                  ),
                  mainPanel(
                    leafletOutput("mapa_covid", height = "500px"),
                    HTML("<br><br><br>"), # para dar um espaço entre os gráficos
                    width = 12
                  )
                ),
                column(
                  width = 3,
                  box(
                    dataTableOutput("table_covid", height = "580px"),
                    width = 12
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  tabBox(id = "tab_covid",
                         width = 12,
                         title = NULL,
                         tabPanel("Diário",
                                  plotlyOutput("serie_covid_dia", height = 450)
                         ),
                         tabPanel("Semana Epidemiológica",
                                  plotlyOutput("serie_covid_sem", height = 450)
                         )
                         
                  )
                )
              )
            )
    ),
    tabItem("mapa_leitos_rs",
            fluidPage(
              
              # pegando de alguma forma htmlzada a versão mais recente dos ícones font awesome
              
              tags$script(src = "https://kit.fontawesome.com/ab15947b75.js", crossorigin="anonymous"), 
              
              titlePanel(
                h1("Mapa com dados dos leitos UTI Adulto no Rio Grande do Sul")
              ),
              fluidRow(
                valueBoxOutput("box_tot", width = 3),
                valueBoxOutput("box_int", width = 3),
                valueBoxOutput("box_lot", width = 3),
                valueBoxOutput("box_cov", width = 3)
              ),
              fluidRow(
                column(
                  width = 9,
                  fluidRow(
                    column(
                      width = 6,
                      h3("Selecione a variável de interesse"),
                      radioButtons("var_leitos",
                                   label = NULL,
                                   choices = list("Leitos totais" = "leitos_total","Leitos disponíveis" = "leitos_disponiveis","Lotação" = "lotacao", "Leitos ocupados COVID-19" = "leitos_covid"),
                                   selected = "leitos_total",
                                   inline = T)
                    ),
                    column(
                      width = 6,
                      h3("Selecione o tipo de agrupamento"),
                      radioButtons("var_leitos_2",
                                   label = NULL,
                                   choices = list("Hospital" = "hospital", "Municípios" = "municipio", "Mesoregiões" = "meso_regiao"),
                                   selected = "municipio",
                                   inline = T),
                    )
                  ),
                  mainPanel(
                    leafletOutput("mapa_leitos", height = "500px"),
                    HTML("<br><br><br>"), # para dar um espaço entre os gráficos
                    width = 12
                  )
                ),
                column(
                  width = 3,
                  box(
                    dataTableOutput("table_leitos", height = "580px"),
                    width = 12
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Série histórica de leitos",
                  plotlyOutput("serie_leitos", height = "500px")
                ),
              )
            )
    ),
    tabItem("fonte",
            fluidPage(
              fluidRow(
                
                # pegando de alguma forma htmlzada a versão mais recente dos ícones font awesome
                
                tags$script(src = "https://kit.fontawesome.com/ab15947b75.js", crossorigin="anonymous"), 
                
                setZoom(id = "dados_covid",class = "small-box"), # dando um zoomzin quando passa o mouse nos links com base de dados
                setZoom(id = "dados_leitos",class = "small-box"),
                
                column(
                  width = 6,
                  valueBoxOutput("dados_covid",width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("dados_leitos",width = 12)
                )
              )
              
              
            )
    ),
    tabItem("sobre",
            fluidPage(
              fluidRow(
                widgetUserBox(
                  title = tags$b("Franciele Lobo Pallaoro"),
                  subtitle = "Estudante de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/franciele.jpg?raw=true',
                  color = "red",
                  "Contato: franpallaoro@gmail.com",
                  footer_padding = F
                ),
                
                widgetUserBox(
                  title = tags$b("Gabriel Holmer Saul"),
                  subtitle = "Estudante de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/gabriel.jpg?raw=true',
                  color = "orange",
                  "Contato: gabrielholmersaul@gmail.com",
                  footer_padding = F
                )
                ,
                
                widgetUserBox(
                  title = tags$b("Gustavo Machado Utpott"),
                  subtitle = "Estudante de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/gustavo.png?raw=true',
                  color = "red",
                  "Contato: gustavo.utpott@gmail.com",
                  footer_padding = F
                ),
                
                widgetUserBox(
                  title = tags$b("Juliana Sena de Souza"),
                  subtitle = "Estudante de Pós-Graduação em Epidemiologia da UFRGS",
                  type = 2,
                  width = 4,
                  src =  'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/juliana.jpeg?raw=true',
                  color = "orange",
                  "Contato: julianass.estatistica@gmail.com",
                  footer_padding = F
                ),
                
                
                widgetUserBox(
                  title = tags$b("Márcia Helena Barbian"),
                  subtitle = "Professora do departamento de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/marcia.png?raw=true',
                  color = "red",
                  "Contato: mhbarbian@gmail.com",
                  footer_padding = F
                ), 
                
                widgetUserBox(
                  title = tags$b("Rodrigo Citton P. dos Reis"),
                  subtitle = "Professor do departamento de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = 'https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/rodrigo.jpg?raw=true',
                  color = "orange",
                  "Contato: citton.padilha@ufrgs.br",
                  footer_padding = F
                ), 
                
                tags$img(src = "https://github.com/franpallaoro/COVID-19/blob/ssjuliana/Dashboard/fotos/logos.png?raw=true", 
                         height = "150", width = "1000")
              )
            )
    )
  )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = "red")

########################################################################
# Server
########################################################################


server <- function(input, output) {
  
  ###############################
  ####### first tabItem #########
  ###############################
  
  # caixas com numeros gerais
  
  # caixa de confirmados
  output$box_conf <- renderValueBox({
    aux <- dados_covid_rs %>%
      filter(is_last) %>%
      filter(place_type == "state")
    
    total <- aux$confirmed
    
    valueBox(
      total,
      "Casos confirmados",
      icon = icon("notes-medical"),
      color = "red" 
    )
  })
  # caixa incidência por 100 mil habitantes
  output$box_inci <- renderValueBox({
    aux <- dados_covid_rs %>%
      filter(is_last) %>%
      filter(place_type=="state")
    
    confirmados_por_100k <- aux$confirmed_per_100k_inhabitants
    
    valueBox(
      round(confirmados_por_100k,2),
      "Casos confirmados por 100 mil habitantes",
      icon = icon("ambulance"),
      color = "orange" 
    )
  })
  # caixa de óbitos
  output$box_obit <- renderValueBox({
    aux <- dados_covid_rs %>%
      filter(is_last) %>%
      filter(place_type == "state")
    
    mortes <- aux$deaths
    
    valueBox(
      mortes,
      "Óbitos",
      icon = icon("heartbeat"),
      color = "lime", 
    )
  })
  # caixas de letalidade
  output$box_leta <- renderValueBox({
    aux <- dados_covid_rs %>%
      filter(is_last) %>%
      filter(place_type == "state") 
    
    letalidade <- aux$death_rate
    
    valueBox(
      paste0(round(letalidade*100,2),"%"),
      "Letalidade",
      icon = icon("heartbeat"),
      color = "purple", 
    )
  })
  
  #####################
  # Mapa_covid
  
  output$mapa_covid <- renderLeaflet({
    
    var <- rlang::sym(input$var_covid)
    
    if(input$var_covid_2=="municipio") {
      aux_mapa <- dados_mapa_rs %>%
        mutate(var = replace_na(!!var, 0))
    } else {
      aux_mapa <- dados_mapa_rs_meso %>%
        mutate(var = replace_na(!!var, 0))
    }
      
    y_quantidade <- aux_mapa$var
    
    variavel <- as.data.frame(aux_mapa)[,input$var_covid_2]
    
    if(input$var_covid == "confirmed") {
      paleta <- "Reds"
      texto <- "Casos confirmados"
    } else if(input$var_covid == "deaths") {
      paleta <- "Greys"
      texto <- "Óbitos confirmados"
    } else if(input$var_covid == "confirmed_per_100k_inhabitants") {
      paleta <- "Oranges"
      texto <- "Casos por 100mil habitantes"
    } else {
      paleta <- "Purples"
      texto <- "Letalidade"
    }
    
    if(input$var_covid != "death_rate") {
      
      # criando intervalo com uma função muito boa
      
      intervalos <- classInt::classIntervals(var = y_quantidade, n = 6, style = "fisher")
      
      intervalos[["brks"]][1:2] <- c(0,1)
      
      
      pal <- colorBin(palette=paleta, domain = y_quantidade, bins = intervalos[["brks"]])
      
      leaflet(aux_mapa) %>%
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
                    label = sprintf("%s - %s", variavel, y_quantidade),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "6px 11px"),
                      textsize = "15px",
                      direction = "auto"))   %>%
        addLegend(pal = pal, values = round(y_quantidade,0), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
          n = length(cuts)
          paste0(round(cuts[-n],0), " &ndash; ", round(cuts[-1],0))},
          title = texto,
          labels = ~variavel,
          position = "bottomright")
      
    } else {
      
      # criando intervalo com uma função muito boa
      
      intervalos <- classInt::classIntervals(var = y_quantidade, n = 6, style = "fisher")
      
      if (input$var_covid_2 == "municipio") {
        intervalos[["brks"]][1:6] <- c(0,0.001,0.03,0.05,0.1,0.5,1)
      }
      
      pal <- colorBin(palette=paleta, domain = y_quantidade, bins = intervalos[["brks"]])
      
      leaflet(aux_mapa) %>%
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
                    label = sprintf("%s - %s", variavel, paste0(100*round(y_quantidade,4),"%")),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "6px 11px"),
                      textsize = "15px",
                      direction = "auto"))   %>%
        addLegend(pal = pal, values = round(y_quantidade,0), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
          n = length(cuts)
          paste0(100*round(cuts[-n],4),"%", " &ndash; ", 100*round(cuts[-1],4),"%")},
          title = texto,
          labels = ~variavel,
          position = "bottomright")
      
    }
    
    
    
  })
  
  #############
  # table_covid
  
  output$table_covid <- renderDataTable({
    
    var <- rlang::sym(input$var_covid)
    
    if(input$var_covid == "confirmed") {
      cor <- "#dd4b39"
      texto <- "Casos confirmados"
    } else if(input$var_covid == "confirmed_per_100k_inhabitants") {
      cor <- "#ff851b"
      texto <- "Confirmados por 100k habitantes"
    } else if(input$var_covid == "deaths"){
      cor <- "#757474"
      texto <- "Óbitos"
    } else {
      cor <- "#605ca8"
      texto <- "Letalidade"
    }
    
    if(input$var_covid_2 == "municipio") {
      
      aux <- dados_covid_rs %>%
        filter(place_type == "city") %>%
        filter(is_last) %>%
        arrange(desc(!!var))
      
      if(input$var_covid == "death_rate") {
        aux <- aux %>%
          filter(!is.na(!!var))
        aux$death_rate <- paste0(100*round(aux$death_rate,4),"%")
      }
  
      tabela <- datatable(
        aux[,c("municipio",input$var_covid)], 
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
        formatStyle("municipio",color = "#222d32", fontSize = "12px", backgroundColor = "#f0f0f0") %>%
        formatStyle(input$var_covid, color = cor, fontWeight = "bold",fontSize = "12px", backgroundColor = "#f0f0f0")
      
      if(input$var_covid == "confirmed_per_100k_inhabitants") {
        tabela <- formatRound(tabela, input$var_covid, digits = 2)
      }
      
      tabela
    
    } else {
      
      aux <- dados_covid_rs %>%
        filter(place_type == "city") %>%
        filter(is_last) %>%
        group_by(mesorregiao) %>%
        summarise(confirmed = sum(confirmed), deaths = sum(deaths), estimated_population_2019 = sum(estimated_population_2019),
                  death_rate = sum(deaths)/sum(confirmed), confirmed_per_100k_inhabitants = sum(confirmed)*100000/sum(estimated_population_2019)) %>%
        arrange(desc(!!var))
      
      if(input$var_covid == "death_rate") {
        aux$death_rate <- paste0(100*round(aux$death_rate,4),"%")
      }
      
      tabela <- datatable(
        aux[,c("mesorregiao",input$var_covid)], 
        rownames=F,
        class = "compact",
        colnames = c("Mesoregião",texto),
        options = list(
          dom = "tS", 
          ordering = F,
          scrollY = "560px",
          paging = FALSE
        )
      ) %>%
        formatStyle("mesorregiao",color = "#222d32", fontSize = "12px", backgroundColor = "#f0f0f0") %>%
        formatStyle(input$var_covid, color = cor, fontWeight = "bold",fontSize = "12px", backgroundColor = "#f0f0f0")
      
      if(input$var_covid == "confirmed_per_100k_inhabitants") {
        tabela <- formatRound(tabela, input$var_covid, digits = 2)
      }
      
      tabela  
      
    }
    
  })
  
  ############
  # serie_covid_dia
  
  output$serie_covid_dia <- renderPlotly({

    var <- rlang::sym(input$var_covid)
      
    aux <- dados_covid_rs %>%
      filter(place_type == "state") %>%
      arrange(date)
    
    if(input$var_covid == "confirmed") {
      cor <- "#dd4b39"
      texto <- "Casos confirmados"
    } else if(input$var_covid == "confirmed_per_100k_inhabitants") {
      cor <- "#ff851b"
      texto <- "Confirmados por 100k habitantes"
    } else {
      cor <- "#757474"
      texto <- "Óbitos"
    }
    
    
    ordem <- as.character(format(aux$date, "%d-%m"))
    
    aux$date <- as.character(format(aux$date, "%d-%m"))
    
    if(input$var_covid %in% c("confirmed","deaths")) {
      
      aux <- as.data.frame(aux)
      
      aux$novos <- c(aux[1,input$var_covid],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- aux[i,input$var_covid]-aux[i-1,input$var_covid]
      }
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = !!var, group = 1), color = cor, linetype = 'dotted') +
        geom_point(aes(x = date, y = !!var), color = cor) + 
        geom_col(data = aux, mapping = aes(x = date, y = novos), fill = cor) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = texto) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
        
    } else {
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = !!var, group = 1), color = cor, linetype = 'dotted') +
        geom_point(aes(x = date, y = !!var), color = cor) + 
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = texto) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
    }
        
    
    
    ggplotly(p) 
    
  })
  
  ############
  # serie_covid_semana
  
  output$serie_covid_sem <- renderPlotly({
    
    var <- rlang::sym(input$var_covid)
    
    if(input$var_covid == "confirmed") {
      cor <- "#dd4b39"
      texto <- "Casos confirmados"
    } else if(input$var_covid == "confirmed_per_100k_inhabitants") {
      cor <- "#ff851b"
      texto <- "Confirmados por 100k habitantes"
    } else {
      cor <- "#757474"
      texto <- "Óbitos"
    } 
      
    aux <- dados_covid_rs %>%
      filter(place_type == "state") %>%
      group_by(semana_epidemiologica) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      arrange(semana_epidemiologica)
    
    ordem <- as.character(aux$semana_epidemiologica)
    
    aux$semana_epidemiologica <- as.character(aux$semana_epidemiologica)
    
    if(input$var_covid %in% c("confirmed","deaths")) {
      
      aux$novos <- c(as.data.frame(aux)[1,input$var_covid],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- as.data.frame(aux)[i,input$var_covid]-as.data.frame(aux)[i-1,input$var_covid]
      }
      
      p <- ggplot(aux) +
        geom_line(aes(x = semana_epidemiologica, y = !!var, group = 1), color = cor, linetype = 'dotted') +
        geom_point(aes(x = semana_epidemiologica, y = !!var), color = cor) + 
        geom_col(aes(x = semana_epidemiologica, y = novos), fill = cor) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Semana Epidemiológica", y = texto) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
    } else {
      
      
      p <- ggplot(aux) +
        geom_line(aes(x = semana_epidemiologica, y = !!var, group = 1), color = cor, linetype = 'dotted') +
        geom_point(aes(x = semana_epidemiologica, y = !!var), color = cor) + 
        scale_x_discrete(limits = ordem) +
        labs(x = "Semana Epidemiológica", y = texto) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
    }
      
    
    ggplotly(p) 
    
  })
  
  ###############################
  ###### secoond tabItem ########
  ###############################
  
  # caixas com numeros gerais
  
  # caixa de leitos totais
  output$box_tot <- renderValueBox({
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao))
    
    valueBox(
      sum(aux$leitos_total),
      "Leitos totais",
      icon = icon("hospital"),
      color = "green" 
    )
  })
  # caixa de internados
  output$box_int <- renderValueBox({
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao))
    
    valueBox(
      sum(aux$leitos_disponiveis),
      "Leitos disponíveis",
      icon = icon("procedures"),
      color = "blue" 
    )
  })
  # caixa de lotação
  output$box_lot <- renderValueBox({
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao))

    valueBox(
      paste0(round(100*sum(aux$leitos_internacoes)/sum(aux$leitos_total),2),"%"),
      "Porcentagem de lotação dos leitos",
      icon = icon("hospital-user"),
      color = "purple", 
    )
  })
  # caixas de leitos covid
  output$box_cov <- renderValueBox({
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao))
    
    valueBox(
      sum(aux$leitos_covid),
      "Leitos ocupados COVID-19",
      icon = icon("virus"),
      color = "maroon", 
    )
  })
  
  #####################
  # Mapa_leitos
  
  output$mapa_leitos <- renderLeaflet({
    
    var <- rlang::sym(input$var_leitos)
    
    if (input$var_leitos_2 == "hospital") {
      if (input$var_leitos != "lotacao") {
        aux_mapa <- leitos_uti %>%
          filter(!is.na(!!var)) %>%
          filter(!!var != 0) %>%
          group_by(cnes, LATITUDE, LONGITUDE, hospital) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          summarise(var = sum(!!var))
      } else {
        aux_mapa <- leitos_uti %>%
          filter(!is.na(!!var)) %>%
          group_by(cnes, LATITUDE, LONGITUDE, hospital) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          summarise(var = ifelse(sum(leitos_total)!=0,sum(leitos_internacoes)/sum(leitos_total),NA))
      }
      
    } else if (input$var_leitos_2 == "municipio") {
      aux_mapa <- leitos_mapa_mun_rs %>%
        mutate(var = !!var)
    } else {
      aux_mapa <- leitos_mapa_meso_rs %>%
        mutate(var = !!var)
    }
    
    y_quantidade <- aux_mapa$var
    
    if (input$var_leitos == "leitos_total") {
      paleta <- "Greens"
      cor <- "#00a65a"
      texto <- "Total de leitos"
    } else if (input$var_leitos == "leitos_disponiveis") {
      paleta <- "Blues"
      cor <- "#0073b7"
      texto <- "Leitos disponíveis"
    } else if(input$var_leitos == "lotacao") {
      paleta <- "Purples"
      cor <- "#605ca8"
      texto <- "Lotação média"
    } else {
      paleta <- "RdPu"
      cor <- "#d81b60"
      texto <- "Leitos ocupados COVID-19"
    }
    
    if(input$var_leitos_2 != "hospital") {
      
      variavel <- as.data.frame(aux_mapa)[,input$var_leitos_2]
      
      if(input$var_leitos != "lotacao") {
        
        y_quantidade <- replace_na(y_quantidade, 0) 
        
        # criando intervalo com uma função muito boa
        
        intervalos <- classInt::classIntervals(var = y_quantidade, n = 6, style = "fisher")
        
        intervalos[["brks"]][1:2] <- c(0,1)
        
        pal <- colorBin(palette=paleta, domain = y_quantidade, bins = intervalos[["brks"]])
        
        leaflet(aux_mapa) %>%
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
                      label = sprintf("%s - %s", variavel, y_quantidade),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "6px 11px"),
                        textsize = "15px",
                        direction = "auto"))   %>%
          addLegend(pal = pal, values = y_quantidade, labFormat = function(type, cuts, p) {  # legenda para colorQuantile
            n = length(cuts)
            paste0(cuts[-n], " &ndash; ", cuts[-1])},
            title = texto,
            labels = ~variavel,
            position = "bottomright")
        
      } else {
        
        paleta <- RColorBrewer::brewer.pal(n=6,"Purples")
        paleta <- paleta[2:6]
        
        pal <- colorBin(palette=paleta, domain = y_quantidade, bins = c(0,0.2,0.4,0.6,0.8,round(max(y_quantidade, na.rm = T),4)), na.color = "#ffffff")
        
        leaflet(aux_mapa) %>%
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
                      label = sprintf("%s - %s", variavel, paste0(100*round(y_quantidade,4),"%")),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "6px 11px"),
                        textsize = "15px",
                        direction = "auto"))   %>%
          addLegend(pal = pal, values = paste0(100*round(y_quantidade,4),"%"), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
            n = length(cuts)
            paste0(cuts[-n]*100,"%", " &ndash; ", cuts[-1]*100,"%")},
            title = texto,
            labels = ~variavel,
            position = "bottomright")
        
      }
      
      
      
    } else {
      
      if(input$var_leitos != "lotacao") {
        
        labs <- lapply(seq(nrow(aux_mapa)), function(i) {
          paste0(aux_mapa[i, "var"],"%", " ",texto, '</p>', 
                 " ",aux_mapa[i, "hospital"]) 
        })
        
        calculo_raio <- 3.5*aux_mapa$var^(1/2)
        
        leaflet(aux_mapa) %>%
          addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
          addCircleMarkers(lng = aux_mapa$LONGITUDE, lat = aux_mapa$LATITUDE, radius = calculo_raio,
                           color = cor, fillOpacity = 0.5, label = lapply(labs, htmltools::HTML), 
                           labelOptions = labelOptions(interactive = T, textsize = "15px"))
        
      } else {
        
        labs <- lapply(seq(nrow(aux_mapa)), function(i) {
          paste0(100*round(aux_mapa[i, "var"],4),"%", " ",texto, '</p>', 
                 " ",aux_mapa[i, "hospital"]) 
        })
        
        calculo_raio <- (10*aux_mapa$var)^(2)/3
        
        leaflet(aux_mapa) %>%
          addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
          addCircleMarkers(lng = aux_mapa$LONGITUDE, lat = aux_mapa$LATITUDE, radius = calculo_raio,
                           color = cor, fillOpacity = 0.5, label = lapply(labs, htmltools::HTML), 
                           labelOptions = labelOptions(interactive = T, textsize = "15px"))
      }
      
      
      
    }
  })
  
  #############
  # table_leitos
  
  output$table_leitos <- renderDataTable({
    
    var <- rlang::sym(input$var_leitos)
    var2 <- rlang::sym(input$var_leitos_2)
    
    if (input$var_leitos_2 != "hospital") {
      if(input$var_leitos != "lotacao") {
        aux <- leitos_uti %>%
          group_by(cnes) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          ungroup() %>%
          select(c(input$var_leitos,input$var_leitos_2))
        
        aux <- aux %>%
          group_by(!!var2) %>%
          summarise(var = sum(!!var)) %>%
          mutate_all(~ replace(., . == 0, NA)) %>%
          filter_all(all_vars(!is.na(.))) %>%
          arrange(desc(var))
      } else {
        aux <- leitos_uti %>%
          group_by(cnes) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          ungroup() %>%
          select(c(leitos_total,leitos_internacoes,input$var_leitos_2))
        
        aux <- aux %>%
          group_by(!!var2) %>%
          summarise(var = ifelse(sum(leitos_total) == 0, NA, sum(leitos_internacoes)/sum(leitos_total))) %>%
          mutate_all(~ replace(., . == 0, NA)) %>%
          filter_all(all_vars(!is.na(.))) %>%
          arrange(desc(var))
      }
      
      
    } else {
      aux <- leitos_uti %>%
        group_by(cnes) %>%
        filter(data_atualizacao == max(data_atualizacao)) %>%
        ungroup() %>%
        select(c(input$var_leitos,"hospital","cnes")) %>%
        mutate(var = !!var) %>%
        filter(!is.na(var)) %>%
        arrange(desc(var))
      
    }
    
    
    if (input$var_leitos == "leitos_total") {
      cor <- "#00a65a"
      texto <- "Total de leitos"
    } else if (input$var_leitos == "leitos_disponiveis") {
      cor <- "#0073b7"
      texto <- "Leitos disponíveis"
    } else if(input$var_leitos == "lotacao") {
      cor <- "#605ca8"
      texto <- "Lotação média"
      aux$var <- paste0(round(aux$var,4)*100,"%")
    } else {
      cor <- "#d81b60"
      texto <- "Leitos ocupados COVID-19"
    }
    
    if(input$var_leitos_2 == "municipio") {
      text2 <- "Município"
    } else if(input$var_leitos_2 == "meso_regiao") {
      text2 <- "Mesoregião"
    } else {
      text2 <- "Hospital"
    }
    
    datatable(
      aux[,c(input$var_leitos_2,"var")], 
      rownames=F,
      class = "compact",
      colnames = c(text2,texto),
      options = list(
        dom = "tS", 
        ordering = F,
        scrollY = "530px",
        paging = FALSE
      )
    ) %>%
      formatStyle(input$var_leitos_2,color = "#787878", fontSize = "12px", backgroundColor = "#f0f0f0") %>%
      formatStyle("var", color = cor, fontWeight = "bold",fontSize = "12px", backgroundColor = "#f0f0f0")
    
  })
  
  #############
  # serie_leitos
  
  output$serie_leitos <- renderPlotly({

    aux <- leitos_uti %>%
      group_by(data_atualizacao) %>%
      summarise(total = sum(leitos_total), disponiveis = sum(leitos_disponiveis), lotacao = sum(leitos_internacoes)/sum(leitos_total),
                covid = sum(leitos_covid)) %>%
      arrange(data_atualizacao)
    
    ordem <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    p <- ggplot(aux) +
      geom_line(aes(x = data_atualizacao, y = total, group = 1), color = "#00a65a") +
      geom_point(aes(x = data_atualizacao, y = total), color = "#00a65a") +
      geom_line(aes(x = data_atualizacao, y = disponiveis, group = 1), color = "#0073b7") +
      geom_point(aes(x = data_atualizacao, y = disponiveis, label = lotacao), color = "#0073b7") +
      geom_line(aes(x = data_atualizacao, y = covid, group = 1), color = "#d81b60") +
      geom_point(aes(x = data_atualizacao, y = covid), color = "#d81b60") +
      scale_x_discrete(limits = ordem) +
      labs(x = "Dia", y = "Número de leitos disponíveis, com COVID-19 e total") +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
      
    ggplotly(p)
    
  })
  

  ###############################
  ######  third tabItem  ########
  ###############################
  
  output$dados_covid <- renderValueBox({
    
    valueBox(
      value = "COVID-19",
      subtitle = "Dados extraídos das Secretarias de Saúde dos estados e reunidos no site Brasil.io",
      icon = icon("viruses"),
      color = "aqua",
      href = "https://brasil.io/dataset/covid19/caso/",
      width = 12
    )
    
  })
  
  output$dados_leitos <- renderValueBox({
    
    valueBox(
      value = "UTI - Adulto",
      subtitle = "Secretaria da Saúde do Rio Grande do Sul",
      icon = icon("hospital"),
      color = "aqua",
      href = "http://ti.saude.rs.gov.br/covid19/leitos/dashboard.php",
      width = 12
    )
    
  })
  
}

shinyApp(ui, server)



