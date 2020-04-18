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

theme_set(theme_gray())
#-------------------------------------
# banco de dados de  casos confirmados:
covid <- readRDS(here::here('data','casos_covid19_br_mun.rds'))

# banco de dados por estado:
data_state <- covid %>%
  select(state, confirmed, deaths, confirmed_per_100k_inhabitants, 
         death_rate, is_last, place_type, estimated_population_2019) %>%
  filter(is_last == 'TRUE' & place_type == 'state')

data_state[27,5] <- ifelse(is.na(data_state[27,5]) == 'TRUE', 0, data_state[27,5])

# banco de dados obitos cartorio:

obitos_cartorio <- readRDS(here::here('data','obitos_br_uf.rds')) %>%
  filter(date >= "2020-03-16") # filtrando a partir do primeiro caso

names(obitos_cartorio) <- c("Estado","Data","Acumulado mortes COVID-19","Acumulado mortes Pneumonia 2019","Acumulado mortes Pneumonia 2020",
                            "Acumulado mortes por falha respiratória 2019","Acumulado mortes por falha respiratória 2020",
                            "Semana_epidemiologica_2019","Semana_epidemiologica_2020","Mortes COVID-19","Mortes Pneumonia 2019",
                            "Mortes Pneumonia 2020","Mortes por falha respiratória 2019","Mortes por falha respiratória 2020")

# banco de dados com o total de casos no brasil por dia: 

pop_br = sum(data_state$estimated_population_2019)

casos_br <- covid %>%
  select(state, confirmed, deaths, 
         date, place_type) %>%
  filter(place_type == 'state') %>%
  aggregate(cbind(confirmed, deaths) ~ date, data = ., sum)

casos_br <- casos_br %>%
  mutate(conf_per100k = confirmed/pop_br*100000) %>%
  mutate(letal = deaths/confirmed) %>%
  select(confirmed, deaths, conf_per100k, letal, date)

# cor e ooções de seleção da variável a ser vista

fcolor <- c("#dd4b39", "#605ca8", "#f39c12", "#d81b60")
select_choices <- c("Casos Confirmados", "Óbitos", "Casos/100k hab.", "Letalidade")

obts <- readRDS(here::here('data', 'obitos_br_uf.rds'))

temp <- obts %>%
  select(date, epidemiological_week_2020)

temp <- merge(casos_br, temp, by = 'date')
temp <- unique(temp)

casos_br <- casos_br %>%
  mutate(ep_week = temp$epidemiological_week_2020)

rm(temp)

# serie temporal com os dados no brasil
plot_geral <- function(input){
  
  temp <- casos_br[,c(which(input == select_choices), 5)] # dado selecionado no input
  temp <- data.frame(Data = as.character(stringr::str_sub(temp$date, 6, 10)), 
                     Frequencia = round(temp[,1], 3))
  
  col_sel <- fcolor[which(input == select_choices)]
  
  if(which(input == select_choices) < 3){
    
    Diario <- c(casos_br[1,which(input == select_choices)], 
                diff(casos_br[,which(input == select_choices)]))
    
    p <- ggplot(temp) +
      geom_line(aes(x = Data, y = Frequencia, group = 1), color = col_sel, linetype = 'dotted') +
      geom_point(aes(x = Data, y = Frequencia), color = col_sel) + 
      geom_bar(aes(x = Data, y = Diario), fill = col_sel, stat = 'identity') + 
      theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5)) + 
      labs(x = NULL, y = paste0(input)) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank())
    
    
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
      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank())
    
    
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
    mutate(variavel = dataset[,which(input == select_choices)]) %>%
    select(id, variavel)
  
  tidy <- dplyr::left_join(abjData::br_uf_map, dataset, by = 'id')
  
  g <- ggplot(tidy) +
    geom_polygon_interactive(aes(long, lat, group = group, fill = variavel, tooltip = variavel),
                             color = 'black') + 
    scale_fill_continuous(name = select_choices[which(input == select_choices)], low = '#ebebeb', 
                          high = fcolor[which(input == select_choices)], na.value = 'white') + 
    theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) + 
    theme_void() + 
    theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank()) 
  
  ggiraph(code = print(g))
  
}

# gráfico de barras por estado
plot_bar <- function(input){
  
  Frequencia = round(as.vector(data_state[,(which(input == select_choices) + 1)]), 2)
  selcor = fcolor[which(input == select_choices)]
  
  
  p = ggplot(data_state, aes(x = state, y = Frequencia)) +
    geom_col(fill = selcor) +
    geom_text(aes(label = Frequencia), size = 3) +
    coord_flip() +
    ylim(0, max(Frequencia) + mean(Frequencia)) + 
    labs(x = NULL, y = paste0(input)) + 
    theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank())
  
  ggplotly(p) %>%
    style(textposition = "middleright") %>%
    layout(yaxis = list(tickmode = 'array', 
                        tickvals = 1:nrow(data_state), 
                        ticktext = unique(data_state$state)))
}

# dados do brasil por semana epi
week_geral <- function(input){
  
  #-- banco de dados para semana epidemiologica
  temp <-  covid %>%
    select(state, confirmed, deaths, 
           date, place_type) %>%
    filter(place_type == 'state') %>%
    aggregate(cbind(confirmed, deaths) ~ date, data = ., sum)
  
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
    
    temp$Acumulado <- cumsum(temp$Frequencia)
    
    p <- ggplot(temp) +
      geom_line(aes(x = ep_week, y = Acumulado, group = 1), color = col_sel, linetype = 'dotted') +
      geom_point(aes(x = ep_week, y = Acumulado), color = col_sel) + 
      geom_bar(aes(x = ep_week, y = Frequencia), fill = col_sel, stat = 'identity') + 
      theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5)) + 
      labs(x = NULL, y = paste0(input)) + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank()) 
    
    ggplotly(p)
    
  } else {
    
    p <- ggplot(temp) +
      geom_line(aes(x = ep_week, y = Frequencia, group = 1), color = col_sel) +
      geom_point(aes(x = ep_week, y = Frequencia, group = 1), color = col_sel) +
      theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5)) + 
      labs(x = NULL, y = paste0(input)) + 
      theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
            panel.grid.major = element_blank())
    ggplotly(p)
    
  }
}

# function plot óbitos do cartório

plot_cart <- function(input) {

  if(input=="Diário") {
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
  
  p1 <- ggplot(aux) +
    geom_line(aes(x = !!var, y = deaths, color = disease_type), linetype = 'dotted') +
    geom_point(aes(x = !!var, y = deaths, color = disease_type)) + 
    theme(axis.text.x = element_text(angle = 45, size = 8, vjust = 0.5)) + 
    scale_color_manual(name = "Doenças", values = paleta) +
    labs(x = text, y = text2) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
          panel.grid.major = element_blank(),
          panel.background = element_rect("transparent", color = NA)) 
  
  if(input=="Diário") {
    p1 <- p1 +
      scale_x_date(date_breaks = "1 day", date_labels = "%m-%d") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  
  ggplotly(p1)
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
      
      menuItem("COVID-19", icon = icon("chart-area"), tabName = "dashbr", 
               badgeLabel = "BR", badgeColor = "teal"),
      menuItem("COVID-19", icon = icon("chart-bar"), tabName = "dashuf", 
               badgeLabel = "UF", badgeColor = "teal"),
      menuItem("Óbitos Cartório", icon = icon("chart-line"), tabName = "cart", badgeLabel = "BR", badgeColor = "teal"),
      menuItem("Fonte de Dados", icon = icon("file-download"), tabName = "dados", badgeColor = "teal"),
      menuItem("CovidMetrika", icon = icon("users"), tabName = "us", badgeColor = "teal"), 
      menuItem("Source Code", icon = icon("code"), badgeColor = "teal", 
               href = 'https://github.com/franpallaoro/COVID-19/')
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
                  
                  ), # coluna 
                
                box(ggiraphOutput("mapaPlot", height = 350L), width = 6L, height = 390L),
                box(plotlyOutput("barPlot", height = 500L), width = 6L, height = 540L)
      ) #fluidrow
      ), # final da parte dos dados nacionais
      
      tabItem("dashuf", "Em construção"),
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
                         
                  ) 
                  
                )
              )
              
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
  # 'output' das caixas de informações principais: 
  
  output$casosBox <- renderValueBox({
    valueBox(casos_br[nrow(casos_br),1], "Casos", icon = icon("ambulance"),
      color = "red"
    )
  })
  
  output$obitosBox <- renderValueBox({
    valueBox(casos_br[nrow(casos_br),2], "Óbitos", icon = icon("skull"),
      color = "purple"
    )
  })
  
  output$taxaBox <- renderValueBox({
    valueBox(round(casos_br[nrow(casos_br),3],2), "Taxa /100k hab.", icon = icon("heartbeat"),
      color = "yellow"
    )
  })
  
  output$letalBox <- renderValueBox({
    valueBox(
      paste0(round(casos_br[nrow(casos_br),4]*100, 2), '%'), 
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
  output$mapaPlot <- renderggiraph({
    plot_mapa(input$typevar)
  })
  
  output$barPlot <- renderPlotly({
    plot_bar(input$typevar)
  })
  
  #-------------------------------------
  
  # tabItem obitos cartorio
  
  # boxes 
  
  output$box_pneumonia <- renderValueBox({
    valueBox(
      value = sum(obitos_cartorio$`Mortes Pneumonia 2020`), 
      subtitle = "Total de óbitos pneumonia desde o primeiro óbito COVID-19", 
      icon = tags$i(class = "fa fa-lungs"),
      color = "navy"
    )
  })
  
  output$box_falha <- renderValueBox({
    valueBox(
      value = sum(obitos_cartorio$`Mortes por falha respiratória 2020`), 
      subtitle = "Total de óbitos falha respiratória desde o primeiro óbito COVID-19", 
      icon = tags$i(class = "fa fa-lungs"),
      color = "olive"
    )
  })
  
  output$box_covid <- renderValueBox({
    valueBox(
      value = sum(obitos_cartorio$`Mortes COVID-19`), 
      subtitle = "Total de óbitos COVID-19", 
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
