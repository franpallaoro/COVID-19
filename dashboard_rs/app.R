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
                                   choices = list("Confirmados" = "confirmed","Confirmados por 100 mil habitantes" = "confirmed_per_100k_inhabitants","Óbitos" = "deaths"),
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
                    background = "red",
                    dataTableOutput("table_covid", height = "580px"),
                    width = 12
                  )
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Série histórica do número de casos",
                  background = "red",
                  plotlyOutput("serie_casos", height = "450px")
                ),
                box(
                  width = 6,
                  title = "Série histórica do número de óbitos",
                  background = "red",
                  plotlyOutput("serie_obitos", height = "450px")
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
                                   choices = list("Total de leitos" = "leitos_total","Leitos ocupados" = "leitos_internacoes","Leitos ocupados COVID-19" = "leitos_covid"),
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
                  width = 6,
                  title = "Série histórica de leitos ocupados",
                  plotlyOutput("serie_leitos_ocupa", height = "450px")
                ),
                box(
                  width = 6,
                  title = "Série histórica leitos ocupados covid-19",
                  plotlyOutput("serie_leitos_covid", height = "450px")
                )
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
                column(
                  width = 12,
                  gradientBox(
                    title = "Fonte de dados",
                    width = 12,
                    icon = "fa fa-file",
                    gradientColor = "yellow", 
                    boxToolSize = "sm", 
                    closable = F,
                    collapsible = T,
                    a(strong("Dados COVID-19"), href = "https://brasil.io/dataset/covid19/caso", style = "color:black"),br(),
                    a(strong("Dados leitos"), href = "http://ti.saude.rs.gov.br/covid19/leitos/", style = "color:black"),br(),
                    a(strong("Dados DATASUS"), href =  "http://www2.datasus.gov.br/DATASUS/index.php?area=0204&id=11663", style = "color:black"),
                    footer = "Dados obtidos pelo DATASUS têm aqui sua última extração disponível que é de fevereiro/2020, já os outros são atualizados diariamente"
                  )
                ),
              ),
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
    } else {
      paleta <- "Oranges"
      texto <- "Casos por 100mil habitantes"
    }
    
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
    
  })
  
  #############
  # table_covid
  
  output$table_covid <- renderDataTable({
    
    var <- rlang::sym(input$var_covid)
    
    if(input$var_covid_2 == "municipio") {
      
      aux <- dados_covid_rs %>%
        filter(place_type == "city") %>%
        filter(is_last) %>%
        arrange(desc(!!var))
      
      texto <- ifelse(
        test = input$var_covid == "confirmed",
        yes = "Confirmados",
        no = ifelse(
          test = input$var_covid == "deaths",
          yes = "Óbitos",
          no = "Confirmados por 100mil habitantes"
        ))
      
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
        formatStyle("municipio",color = "#e0e1e2", fontSize = "12px", backgroundColor = "#222d32") %>%
        formatStyle(input$var_covid, color = "#dd4b39", fontWeight = "bold",fontSize = "12px", backgroundColor = "#222d32")
      
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
      
      texto <- ifelse(
        test = input$var_covid == "confirmed",
        yes = "Confirmados",
        no = ifelse(
          test = input$var_covid == "deaths",
          yes = "Óbitos",
          no = "Confirmados por 100mil habitantes"
        ))
      
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
        formatStyle("mesorregiao",color = "#e0e1e2", fontSize = "12px", backgroundColor = "#222d32") %>%
        formatStyle(input$var_covid, color = "#dd4b39", fontWeight = "bold",fontSize = "12px", backgroundColor = "#222d32")
      
      if(input$var_covid == "confirmed_per_100k_inhabitants") {
        tabela <- formatRound(tabela, input$var_covid, digits = 2)
      }
      
      tabela  
      
    }
    
  })
  
  ############
  # serie_casos
  
  output$serie_casos <- renderPlotly({
    
    if(input$var_covid_2 == "municipio") {
      
      aux <- dados_covid_rs %>%
        filter(place_type == "city") %>%
        group_by(date) %>%
        summarise(acumulado = sum(confirmed)) %>%
        arrange(date)
      
      ordem <- as.character(format(aux$date, "%d-%m"))
      
      aux$novos <- c(aux$acumulado[1],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- aux$acumulado[i]-aux$acumulado[i-1]
      }
      
      aux$date <- as.character(format(aux$date, "%d-%m"))
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = acumulado, group = 1)) +
        geom_point(aes(x = date, y = acumulado), size=2) +
        geom_col(aes(x = date, y = novos), fill = "#d95f02") +
        geom_text(aes(x = date, y = novos, label = novos)) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = "Casos confirmados") +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
      
      ggplotly(p) %>%
        style(textposition = "top")
      
    } else {
      
      aux <- dados_covid_rs %>%
        filter(place_type == "city") %>%
        group_by(date,mesorregiao) %>%
        summarise(acumulado = sum(confirmed)) %>%
        arrange(date)
      
      ordem <- as.character(format(aux$date, "%d-%m"))
      
      aux$novos <- c(aux$acumulado[1],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- aux$acumulado[i]-aux$acumulado[i-1]
      }
      
      aux$date <- as.character(format(aux$date, "%d-%m"))
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = acumulado, color = mesorregiao, group = mesorregiao), linetype = "dotted") +
        geom_point(aes(x = date, y = acumulado, color = mesorregiao), size=2) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = "Casos confirmados", color = "Mesoregião") +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
      
      ggplotly(p) 
      
    }
    
    
    
  })
  
  ############
  # serie_obitos
  
  output$serie_obitos <- renderPlotly({
    
    if(input$var_covid_2 == "municipio") {
      
      aux <- dados_covid_rs %>%
        filter(place_type == "city") %>%
        group_by(date) %>%
        summarise(acumulado = sum(deaths)) %>%
        arrange(date)
      
      ordem <- as.character(format(aux$date, "%d-%m"))
      
      aux$novos <- c(aux$acumulado[1],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- aux$acumulado[i]-aux$acumulado[i-1]
      }
      
      aux$date <- as.character(format(aux$date, "%d-%m"))
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = acumulado, group = 1)) +
        geom_point(aes(x = date, y = acumulado), size=2) +
        geom_col(aes(x = date, y = novos), fill = "#d95f02") +
        geom_text(aes(x = date, y = novos, label = novos)) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = "Óbitos confirmados") +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
      
      ggplotly(p) %>%
        style(textposition = "top")
      
    } else {
      aux <- dados_covid_rs %>%
        filter(place_type == "city") %>%
        group_by(date, mesorregiao) %>%
        summarise(acumulado = sum(deaths)) %>%
        arrange(date)
      
      ordem <- as.character(format(aux$date, "%d-%m"))
      
      aux$novos <- c(aux$acumulado[1],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- aux$acumulado[i]-aux$acumulado[i-1]
      }
      
      aux$date <- as.character(format(aux$date, "%d-%m"))
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = acumulado, color = mesorregiao, group = mesorregiao), linetype = "dotted") +
        geom_point(aes(x = date, y = acumulado, color = mesorregiao), size=2) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = "Óbitos confirmados" , color = "Mesoregião") +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
      
      ggplotly(p)
    }

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
      color = "blue" 
    )
  })
  # caixa de internados
  output$box_int <- renderValueBox({
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao))
    
    valueBox(
      sum(aux$leitos_internacoes),
      "Leitos ocupados",
      icon = icon("procedures"),
      color = "purple" 
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
      color = "maroon", 
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
      color = "fuchsia", 
    )
  })
  
  #####################
  # Mapa_leitos
  
  output$mapa_leitos <- renderLeaflet({
    
    var <- rlang::sym(input$var_leitos)
    
    if (input$var_leitos_2 == "hospital") {
      aux_mapa <- leitos_uti %>%
        filter(!is.na(!!var)) %>%
        filter(!!var != 0) %>%
        group_by(cnes, LATITUDE, LONGITUDE, hospital) %>%
        filter(data_atualizacao == max(data_atualizacao)) %>%
        summarise(var = sum(!!var))
    } else if (input$var_leitos_2 == "municipio") {
      aux_mapa <- leitos_mapa_mun_rs %>%
        mutate(var = !!var)
    } else {
      aux_mapa <- leitos_mapa_meso_rs %>%
        mutate(var = !!var)
    }
    
    y_quantidade <- aux_mapa$var
    
    if (input$var_leitos == "leitos_total") {
      paleta <- "Blues"
      cor <- "#0073b7"
      texto <- "Total de leitos"
    } else if (input$var_leitos == "leitos_internacoes") {
      paleta <- "Purples"
      cor <- "#605ca8"
      texto <- "Leitos ocupados"
    } else {
      paleta <- "RdPu"
      cor <- "#f012be"
      texto <- "Leitos ocupados COVID-19"
    }
    
    if(input$var_leitos_2 != "hospital") {
      
      variavel <- as.data.frame(aux_mapa)[,input$var_leitos_2]
      
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
      
      labs <- lapply(seq(nrow(aux_mapa)), function(i) {
        paste0(aux_mapa[i, "var"], " ",texto, '</p>', 
               " ",aux_mapa[i, "hospital"]) 
      })
      
      calculo_raio <- 3.5*aux_mapa$var^(1/2)
      
      leaflet(aux_mapa) %>%
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
        addCircleMarkers(lng = aux_mapa$LONGITUDE, lat = aux_mapa$LATITUDE, radius = calculo_raio,
                         color = cor, fillOpacity = 0.5, label = lapply(labs, htmltools::HTML), 
                         labelOptions = labelOptions(interactive = T, textsize = "15px"))
      
    }
  })
  
  #############
  # table_leitos
  
  output$table_leitos <- renderDataTable({
    
    var <- rlang::sym(input$var_leitos)
    ifelse(input$var_leitos_2=="hospital",var2 <- rlang::sym("cnes"),var2 <- rlang::sym(input$var_leitos_2))
    
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao)) %>%
      select(c(input$var_leitos_2,input$var_leitos))
    
    aux <- aux %>%
      group_by(!!var2) %>%
      summarise(var = sum(!!var)) %>%
      mutate_all(~ replace(., . == 0, NA)) %>%
      filter_all(all_vars(!is.na(.))) %>%
      arrange(desc(var))
    
    if(input$var_leitos == "leitos_total") {
      text <- "Leitos UTI - Adulto"
      cor <- "#0073b7"
    } else if(input$var_leitos == "leitos_internacoes") {
      text <- "Internações UTI - Adulto"
      cor <- "#605ca8"
    } else {
      text <- "Internações COVID-19"
      cor <- "#f012be"
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
      colnames = c(text2,text),
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
  # serie_leitos_ocupa
  
  output$serie_leitos_ocupa <- renderPlotly({

    aux <- leitos_uti %>%
      group_by(data_atualizacao) %>%
      summarise(total = sum(leitos_total), internacoes = sum(leitos_internacoes), lotacao = sum(leitos_internacoes)/sum(leitos_total))
    
    ordem <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    p <- ggplot(aux) +
      geom_line(aes(x = data_atualizacao, y = total, group = 1), color = "#0073b7") +
      geom_point(aes(x = data_atualizacao, y = total), color = "#0073b7") +
      geom_col(aes(x = data_atualizacao, y = internacoes, label = lotacao), fill = "#605ca8") +
      scale_x_discrete(limits = ordem) +
      labs(x = "Dia", y = "Número de leitos ocupados e total") +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
      
    ggplotly(p)
    
  })
  
  #############
  # serie_leitos_covid
  
  output$serie_leitos_covid <- renderPlotly({

    aux <- leitos_uti %>%
      group_by(data_atualizacao) %>%
      summarise(var = sum(leitos_covid))
    
    ordem <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))
    
    p <- ggplot(aux) +
      geom_col(aes(x = data_atualizacao, y = var), fill = "#f012be") +
      scale_x_discrete(limits = ordem) +
      labs(x = "Dia", y = "Número de leitos com pacientes COVID-19") +
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



