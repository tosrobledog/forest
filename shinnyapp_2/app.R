#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram

library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        # Custom CSS to hide the default logout panel
        tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
        
        # The dynamically-generated user panel
        uiOutput("userpanel")
    ),
    dashboardBody()
)

server <- function(input, output, session) {
    output$userpanel <- renderUI({
        # session$user is non-NULL only in authenticated sessions
        if (!is.null(session$user)) {
            sidebarUserPanel(
                span("Logged in as ", session$user),
                subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
        }
    })
}

shinyApp(ui, server)