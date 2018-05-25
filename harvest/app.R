## app.R ##
library(shinydashboard)
library(shiny)
library(igraph)
library(tidyverse)
library(ggplot2)
library(DT)
library(reshape2)

paper <- read_csv("forest/paper.csv")
author <- read_csv("forest/author.csv")
conference <- read_csv("forest/conference.csv")
journal <- read_csv("forest/journal.csv")
address <- read_csv("forest/address.csv")
paperauthor <- read_csv("forest/paperauthor.csv")
paperpublisher <- read_csv("forest/paperpublisher.csv")
publisher <- read_csv("forest/publisher.csv")
referencelink <- read_csv("forest/referencelink.csv")

ui <- dashboardPage(
    dashboardHeader(title = "Harvest!",
                    dropdownMenu(type = "messages",
                                 messageItem(
                                     from = "Sales Dept",
                                     message = "Sales are steady this month."
                                 ),
                                 messageItem(
                                     from = "New User",
                                     message = "How do I register?",
                                     icon = icon("question"),
                                     time = "13:45"
                                 ),
                                 messageItem(
                                     from = "Support",
                                     message = "The new server is ready.",
                                     icon = icon("life-ring"),
                                     time = "2014-12-01"
                                 )
                    ),
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                     text = "5 new users today",
                                     icon("users")
                                 ),
                                 notificationItem(
                                     text = "12 items delivered",
                                     icon("truck"),
                                     status = "success"
                                 ),
                                 notificationItem(
                                     text = "Server load at 86%",
                                     icon = icon("exclamation-triangle"),
                                     status = "warning"
                                 )
                    ),
                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                 taskItem(value = 90, color = "green",
                                          "Documentation"
                                 ),
                                 taskItem(value = 17, color = "aqua",
                                          "Project X"
                                 ),
                                 taskItem(value = 75, color = "yellow",
                                          "Server deployment"
                                 ),
                                 taskItem(value = 80, color = "red",
                                          "Overall project"
                                 )
                    )
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Area", tabName = "area", icon = icon("th")),
            menuItem("Papers", tabName = "papers", icon = icon("th")),
            menuItem("Journals", tabName = "journal", icon = icon("th")),
            menuItem("Authors", tabName = "authors", icon = icon("th"))
            
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "area",
                    fluidRow(
                        
                        box(title = "AMID importance", background = "maroon", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("plot1", height = 250))
                    )

            ),
            
            # Second tab content
            tabItem(tabName = "papers",
                    fluidRow(
                        DT::dataTableOutput("paper")
                    )
            ),
            
            # Third tabl content
            tabItem(tabName = "journal",
                    fluidRow(
                        DT::dataTableOutput("journal")
                    )
            ),
            
            # Third tabl content
            tabItem(tabName = "authors",
                    fluidRow(
                        DT::dataTableOutput("authors")
                    )
            )
        )
    )
)

server <- function(input, output) {
    
    
    output$plot1 <- renderPlot({
        referencelink.complete <- referencelink %>% na.omit()
        
        graph <- igraph::graph.data.frame(referencelink.complete, directed = TRUE)
        
        citaciones <- data.frame(id = V(graph)$name, 
                                 citaciones = degree(graph, 
                                                     mode = "in"),
                                 referencias = degree(graph, 
                                                      mode = "out"),
                                 stringsAsFactors = FALSE)
        
        df_paper <- paper %>% left_join(citaciones)
        
        df_paper %>%
            select(year_published, citaciones, referencias) %>%
            group_by(year_published) %>%
            na.omit() %>%
            summarise(total_articulos = n(),
                      total_citaciones = sum(citaciones), 
                      total_referencias = sum(referencias))
        
        df_paper_plot_1 <- 
            
            df_paper %>%
            select(year_published, citaciones, referencias) %>%
            group_by(year_published) %>%
            na.omit() %>%
            summarise(total_articulos = n(),
                      total_citaciones = sum(citaciones), 
                      total_referencias = sum(referencias)) %>%
            mutate(total_articulos_sc = scale(total_articulos),
                   total_citaciones_sc = scale(total_citaciones),
                   total_referencias_sc = scale(total_referencias)) %>%
            select(year_published, total_articulos_sc, total_citaciones_sc, total_referencias_sc)
        
        df_paper_plot_1_melt <- melt(df_paper_plot_1, id = "year_published")
        
        ggplot(data = df_paper_plot_1_melt, aes(x = year_published, y = value, colour = variable)) + geom_line()
        
    })
    
    output$paper <- DT::renderDataTable(DT::datatable({
        
        referencelink.complete <- referencelink %>% na.omit()
        
        graph <- igraph::graph.data.frame(referencelink.complete, directed = TRUE)
        
        citaciones <- data.frame(id = V(graph)$name, 
                                 citaciones = degree(graph, 
                                                     mode = "in"),
                                 referencias = degree(graph, 
                                                      mode = "out"),
                                 stringsAsFactors = FALSE)
        
        df_paper <- paper %>% left_join(citaciones)
        
        df_paper %>%
            select(year_published, citaciones, referencias) %>%
            group_by(year_published) %>%
            na.omit() %>%
            summarise(total_articulos = n(),
                      total_citaciones = sum(citaciones), 
                      total_referencias = sum(referencias))
        
        df_paper %>% 
            select(id, citaciones) %>% 
            group_by(id) %>% 
            summarise(mas_citado = sum(citaciones,
                                       na.rm = TRUE)) %>%
            top_n(n = 5)
    }))
    
    output$journal <- DT::renderDataTable(DT::datatable({
        
        referencelink.complete <- referencelink %>% na.omit()
        
        graph <- igraph::graph.data.frame(referencelink.complete, directed = TRUE)
        
        citaciones <- data.frame(id = V(graph)$name, 
                                 citaciones = degree(graph, 
                                                     mode = "in"),
                                 referencias = degree(graph, 
                                                      mode = "out"),
                                 stringsAsFactors = FALSE)
        
        df_paper <- paper %>% left_join(citaciones)
        
        df_paper %>%
            select(year_published, citaciones, referencias) %>%
            group_by(year_published) %>%
            na.omit() %>%
            summarise(total_articulos = n(),
                      total_citaciones = sum(citaciones), 
                      total_referencias = sum(referencias))
        
        df_paper %>% 
            filter(year_published == 2004) %>%
            select(id_journal, citaciones, referencias) %>%
            na.omit() %>% 
            group_by(id_journal) %>%
            summarise(total_citaciones = sum(citaciones),
                      total_referencias = sum(referencias)) %>%
            arrange(desc(total_citaciones))
    }))
    
    output$authors <- DT::renderDataTable(DT::datatable({
        
        referencelink.complete <- referencelink %>% na.omit()
        
        graph <- igraph::graph.data.frame(referencelink.complete, directed = TRUE)
        
        citaciones <- data.frame(id = V(graph)$name, 
                                 citaciones = degree(graph, 
                                                     mode = "in"),
                                 referencias = degree(graph, 
                                                      mode = "out"),
                                 stringsAsFactors = FALSE)
        
        df_paper <- paper %>% left_join(citaciones)
        
        df_paper %>%
            select(year_published, citaciones, referencias) %>%
            group_by(year_published) %>%
            na.omit() %>%
            summarise(total_articulos = n(),
                      total_citaciones = sum(citaciones), 
                      total_referencias = sum(referencias))
        
        df_paper %>% 
            select(id, citaciones) %>%
            right_join(paperauthor, by = c("id" = "id_paper")) %>%
            na.omit() %>% 
            group_by(id_author) %>%
            summarise(total_citaciones = sum(citaciones)) %>%
            arrange(desc(total_citaciones)) %>%
            head()
    }))
}

shinyApp(ui, server)
