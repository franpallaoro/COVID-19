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
    menuItem("Mapa COVID-19 RS", tabName = "mapa_covid_rs"),
    menuItem("Mapa Saúde RS", tabName = "mapa_saude_rs"),
    menuItem("Sobre o app", tabName = "sobre")
  ),
  width = 180
)


body <- dashboardBody(
  tabItems(
    tabItem("mapa_covid_rs",
            fluidPage(
              titlePanel(
                div(fluidRow(column(width = 6, h1("Mapa com dados do COVID-19 Rio Grande do Sul")
                    ),
                    column(width = 6, 
                           tags$img(src = "ufrgs_logo.png", height = "100", width = "130"),
                           tags$img(src = "logo_ime2.png", height = "100", width = "400"))))
              ),
              fluidRow(
                valueBoxOutput("box_conf", width = 3),
                valueBoxOutput("box_inci", width = 3),
                valueBoxOutput("box_obit", width = 3),
                valueBoxOutput("box_leta", width = 3)
              ),
              fluidRow(
                column(
                  width = 9,
                  h3("Selecione a variável de interesse para o dashboard"),
                  radioButtons("var_covid",
                               label = NULL,
                               choices = list("Confirmados" = "confirmed","Confirmados por 100 mil habitantes" = "confirmed_per_100k_inhabitants","Óbitos" = "deaths"),
                               selected = "confirmed",
                               inline = T),
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
    tabItem("mapa_saude_rs",
            fluidPage(
              titlePanel(
                div(column(width = 6, h1("Mapa com dados de saúde do Rio Grande do Sul")
                ),
                column(width = 6, 
                       tags$img(src = "ufrgs_logo.png", height = "100", width = "130"),
                       tags$img(src = "logo_ime2.png", height = "100", width = "400")))
              ),
              fluidRow(
                column(
                  width = 9,
                  h3("Selecione a variável de interesse para o dashboard"),
                  radioButtons("var_saude",
                               label = NULL,
                               choices = list("UTI Adulto" = "uti_adulto",
                                              "UTI Pediátrico" = "uti_pediatrico",
                                              "Equipamentos de manutenção de vida" = "equip_vida",
                                              "Ventiladores Mecânico" = "ventilador_mecanico"),
                               selected = "uti_adulto",
                               inline = T),
                  mainPanel(
                    leafletOutput("mapa_saude", height = "500px"),
                    HTML("<br><br><br>"), # para dar um espaço entre os gráficos
                    width = 12
                  )
                ),
                column(
                  width = 3,
                  box(
                    background = "red",
                    dataTableOutput("table_saude", height = "580px"),
                    width = 12
                  )
                )
              ),
            )
    ),
    tabItem("sobre",
            fluidPage(
              titlePanel("Sobre o aplicativo"),
              fluidRow(
                column(
                  width = 6,
                  gradientBox(
                    title = "Fonte de dados",
                    width = 12,
                    icon = "fa fa-file",
                    gradientColor = "teal", 
                    boxToolSize = "sm", 
                    closable = F,
                    collapsible = T,
                    a(strong("Dados Covid-19 POA"), href = "https://prefeitura.poa.br/coronavirus", style = "color:black"),br(),
                    a(strong("Dados UTI"), href =  "https://docs.google.com/spreadsheets/d/e/2PACX-1vTwlPdNvBfVAYGVXKzmWxWLpQLuPwf28zVY3PhqBw5qZ6D4sppyXj5IIslEOUfBBipAyqnGTUHX-IRV/pubhtml", style = "color:black"),br(),
                    a(strong("Dados Emergências"), href = "https://docs.google.com/spreadsheets/d/1-Zea1tEJd-rJJp77Veptkrone0_hddVKgy-pW58E5iM/pubhtml#", style = "color:black"),
                    footer = "Dados são atualizados diariamente a partir das 20h"
                  )
                ),
                column(
                  width = 6, 
                  tags$img(src = "ufrgs_logo.png", height = "100", width = "130"),
                  tags$img(src = "logo_ime2.png", height = "100", width = "400"))
              ),
              widgetUserBoxx(
                title = "Gustavo Machado Utpott",
                subtitle = "Densenvolvedor do aplicativo",
                type = NULL,
                width = 4,
                src = NULL,
                color = "red",
                "Estudante de Estatística da UFRGS",
                footer = "Contato: gustavo.utpott@gmail.com"
              ),
              widgetUserBoxx(
                title = "Márcia Helena Barbian",
                subtitle = "Densenvolvedora do aplicativo",
                type = NULL,
                width = 4,
                src = NULL,
                color = "red",
                "Professora do Departamento de Estatística da UFRGS",
                footer = "Contato: mhbarbian@gmail.com"
              ),
              widgetUserBoxx(
                title = "Franciele Lobo Pallaoro",
                subtitle = "Densenvolvedora do aplicativo",
                type = NULL,
                width = 4,
                src = NULL,
                color = "red",
                "Estudante de Estatística da UFRGS",
                footer = "Contato: mhbarbian@gmail.com"
              ),
              widgetUserBoxx(
                  title = "Juliana Sena Souza",
                subtitle = "Densenvolvedora do aplicativo",
                type = NULL,
                width = 4,
                src = NULL,
                color = "red",
                "Estudante do PPGEST da UFRGS",
                footer = "Contato: mhbarbian@gmail.com"
              ),
              widgetUserBoxx(
                title = "Rodrigo Citton Padilha dos Reis",
                subtitle = "Densenvolvedor do aplicativo",
                type = NULL,
                width = 4,
                src = NULL,
                color = "red",
                "Professor do Departamento de Estatística da UFRGS",
                footer = "Contato: mhbarbian@gmail.com"
              ),
              widgetUserBoxx(
                title = "Gabriel Holmer Saul",
                subtitle = "Densenvolvedor do aplicativo",
                type = NULL,
                width = 4,
                src = NULL,
                color = "red",
                "Estudante de Estatística da UFRGS",
                footer = "Contato: mhbarbian@gmail.com"
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
      color = "red" 
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
      color = "purple", 
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
    
    aux_mapa <- dados_mapa_rs %>%
      mutate(var = replace_na(!!var, 0))
    
    y_quantidade <- aux_mapa$var
    
    if(input$var_covid == "confirmed") {
      bins <- c(0, 1, 2, 3, 10, 20, 30, 40, max(y_quantidade, na.rm=T))
      text1 <- sprintf("%s - numero de casos %s", aux_mapa$municip, round(y_quantidade, 2))
      text2 <- "Casos confirmados"
    } else if(input$var_covid == "deaths") {
      bins <- c(0,1,2,3,max(y_quantidade, na.rm=T))
      text1 <- sprintf("%s - numero de óbitos %s", aux_mapa$municip, round(y_quantidade, 2))
      text2 <- "Óbitos confirmados"
    } else {
      bins <- c(0,5,10,20,30,max(y_quantidade, na.rm=T))
      text1 <- sprintf("%s - casos por 100mil habitantes %s", aux_mapa$municip, round(y_quantidade, 2))
      text2 <- "Casos por 100mil habitantes"
    }
    
    pal <- colorBin("YlOrRd", domain = y_quantidade, bins = bins)
    
    leaflet(aux_mapa) %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
      addPolygons(fillColor = ~pal(y_quantidade), 
                  weight = 1,
                  opacity = 0.5,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = text1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~y_quantidade, opacity = 0.7, title = text2,
                labFormat = labelFormat(digits = 5),
                position = "bottomright")
  })
  
  #############
  # table_covid
  
  output$table_covid <- renderDataTable({
    
    var <- rlang::sym(input$var_covid)
    
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
    
  })
  
  ############
  # serie_casos
  
  output$serie_casos <- renderPlotly({
    
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
    
  })
  
  ############
  # serie_obitos
  
  output$serie_obitos <- renderPlotly({
    
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
    
  })
  
  ###############################
  ###### secoond tabItem ########
  ###############################
  
  #####################
  # Mapa_saude
  
  output$mapa_saude <- renderLeaflet({
    
    var <- rlang::sym(input$var_saude)
    
    aux_mapa <- dados_mapa_rs %>%
      mutate(var = replace_na(!!var, 0))
    
    y_quantidade <- aux_mapa$var
    
    if(input$var_saude == "uti_adulto") {
      bins <- c(0, 10, 20, 30, 40, 50, max(y_quantidade, na.rm=T))
      text1 <- sprintf("%s - Leitos UTI adulto %s", aux_mapa$municip, round(y_quantidade, 2))
      text2 <- "UTI - Adulto"
    } else if(input$var_saude == "uti_pediatrico") {
      bins <- c(0,5,10,15,max(y_quantidade, na.rm=T))
      text1 <- sprintf("%s - Leitos UTI pediátrico %s", aux_mapa$municip, round(y_quantidade, 2))
      text2 <- "UTI - Pediátrico"
    } else if(input$var_saude == "equip_vida"){
      bins <- c(0,50,100,250,500,1000,2000,max(y_quantidade, na.rm=T))
      text1 <- sprintf("%s - Equipamentos de manutenção da vida %s", aux_mapa$municip, round(y_quantidade, 2))
      text2 <- "Equipamentos de manutenção da vida"
    } else {
      bins <- c(0,10,25,50,100,200,max(y_quantidade, na.rm=T))
      text1 <- sprintf("%s - Ventiladores mecânicos %s", aux_mapa$municip, round(y_quantidade, 2))
      text2 <- "Ventiladores Mecânicos"
    }
    
    pal <- colorBin("YlOrRd", domain = y_quantidade, bins = bins)
    
    leaflet(aux_mapa) %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
      addPolygons(fillColor = ~pal(y_quantidade), 
                  weight = 1,
                  opacity = 0.5,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = text1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~y_quantidade, opacity = 0.7, title = text2,
                labFormat = labelFormat(digits = 5),
                position = "bottomright")
  })
  
  #############
  # table_saude
  
  output$table_saude <- renderDataTable({
    
    var <- rlang::sym(input$var_saude)
    
    aux <- tibble(dados_mapa_rs$municipio,)
    
    aux <- dados_mapa_rs %>%
      select(c("municipio",input$var_saude))
    
    st_geometry(aux) <- NULL # para tirar a classe de sf que não irei utilizar aqui
    
    aux <- aux %>%
      mutate_all(~ replace(., . == 0, NA)) %>%
      filter_all(all_vars(!is.na(.))) %>%
      arrange(desc(!!var))
    
    if(input$var_saude == "uti_adulto") {
      text <- "UTI - Adulto"
    } else if(input$var_saude == "uti_pediatrico") {
      text <- "UTI - Pediátrico"
    } else if(input$var_saude == "equip_vida"){
      text <- "Equipamentos de manutenção da vida"
    } else {
      text <- "Ventiladores Mecânicos"
    }
    
    datatable(
      aux[,c("municipio",input$var_saude)], 
      rownames=F,
      class = "compact",
      colnames = c("Município",text),
      options = list(
        dom = "tS", 
        ordering = F,
        scrollY = "560px",
        paging = FALSE
      )
    ) %>%
      formatStyle("municipio",color = "#e0e1e2", fontSize = "12px", backgroundColor = "#222d32") %>%
      formatStyle(input$var_saude, color = "#dd4b39", fontWeight = "bold",fontSize = "12px", backgroundColor = "#222d32")
    
  })
}

shinyApp(ui, server)



