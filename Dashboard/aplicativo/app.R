#-------------------------------------
# carregando os pacotes:

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(dplyr)
library(ggplot2)
library(here)
library(plotly)

theme_set(theme_bw())
#-------------------------------------
# banco de dados de 
# casos confirmados:
covid <- readRDS(here::here('data', 'casos_covid19_br_mun.rds'))

# banco de dados com o total 
# mais atualizado e separados 
# por estado:
data_state <- covid %>%
  select(state, confirmed, deaths, confirmed_per_100k_inhabitants, 
         death_rate, is_last, place_type) %>%
  filter(is_last == 'TRUE' & place_type == 'state')

# banco de dados com o total 
# de casos no brasil por dia: 

casos_br <- covid %>%
  select(state, confirmed, deaths, estimated_population_2019, 
         date, place_type) %>%
  filter(place_type == 'state') %>%
  aggregate(cbind(confirmed, deaths, estimated_population_2019) ~ date, data = ., sum)

# funcao para definir qual cor usar
# de acordo com as três do layout:

fcolor <- function(name){
  if(name == 'olive'){
    paste0('#3d9970')
    } else {
    if(name == 'light-blue'){
      paste0('#3c8dbc')
    } else {
      if(name == 'teal'){
        paste0('#39cccc')
      } else {
        paste0('cor inválida')
      }
    }
  }
}

# funcao para serie temporal com 
# dados nacionais:

plot_geral <- function(vary){
  
  yname <- ifelse(vary == 'confirmed', 'Casos Confirmados', 'Óbitos')
  vary <- as.vector(unlist(casos_br[paste0(vary)]))
  col_sel <- fcolor('olive')
  
  temp <- data.frame(Data = as.Date(casos_br$date),
                     Frequencia = vary)
  
  ggplotly(ggplot(temp, aes(x = Data, y = Frequencia)) +
    geom_bar(stat = "identity", fill = col_sel) + 
    geom_line(linetype = 'dashed') + 
    labs(x = NULL, y = paste0(yname)))
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
      ,badgeBackColor = "#40E0D0"
      ,badgeBorderRadius = 3
    )
  ),
  
  #-------------------------------------
  # Menu do dashboard:
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard", 
               badgeLabel = "Brasil", badgeColor = "green"),
      menuItem("Dashboard", icon = icon("th"), tabName = "widgets",
               badgeLabel = "Regional", badgeColor = "green")
    )
  ),
  
  #-------------------------------------
  # 'corpo' do dashboard: 
  
  dashboardBody(
    #-------------------------------------
    # tema do dashboard: 
    
    shinyDashboardThemes(
      theme = "grey_light"
      ),
    
    fluidRow(
      #-------------------------------------
      # as três 'caixas' com informações resumo: 
      
      valueBoxOutput("casosBox"),
      valueBoxOutput("obitosBox"),
      valueBoxOutput("letalBox"),
      
      #-------------------------------------
      # gráfico de confirmados no BR: 
      column(width = 12,
             box(width = NULL, plotlyOutput("confPlot"))
             )
      )
  )
)
#-------------------------------------


#-------------------------------------
server <- function(input, output) {
  #-------------------------------------
  # 'output' das caixas de informações principais: 
  
  output$casosBox <- renderValueBox({
    valueBox(
      data_state %>%
        select(confirmed) %>%
        sum, "Casos", icon = icon("ambulance"),
      color = "olive"
    )
  })
  
  output$obitosBox <- renderValueBox({
    valueBox(
      data_state %>%
        select(deaths) %>%
        sum, "Óbitos", icon = icon("heartbeat"),
      color = "light-blue"
    )
  })
  
  output$letalBox <- renderValueBox({
    valueBox(
      paste0(round(sum(data_state$deaths)/sum(data_state$confirmed)*100, 2), '%'), 
      "Letalidade", icon = icon("exclamation-circle"),
      color = "teal"
    )
  })
  #-------------------------------------
  # output de casos confirmados de COVID-19
  output$confPlot <- renderPlotly({
    plot_geral('confirmed')
  })
  
  #-------------------------------------
}

shinyApp(ui = ui, server = server)

