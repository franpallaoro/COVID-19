#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(shinyWidgets)

logo_blue_gradient <- shinyDashboardLogoDIY(
  
  boldText = "COVID"
  ,mainText = "-19"
  ,badgeText = "CovidMetrika"
  ,textSize = 16
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
  
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = logo_blue_gradient
    ),
  dashboardSidebar(),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    )
  ),
  title = "Dashboard example"
)

server <- function(input, output) {
   
}

# Run the application 
shinyApp(ui = ui, server = server)

