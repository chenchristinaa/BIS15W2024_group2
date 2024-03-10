library(tidyverse)
library(shiny)
library(shinydashboard)

spacemission_fixed <- read_csv("data/space_missions_fixed.csv") %>% 
  clean_names() %>%
  separate(location, into = c("site1", "site2", "site3", "site4", "area", "country"), extra = "drop", fill = "left", sep = ",") %>%
  select(-site1, -site2, -site3, -site4) %>%
  separate(date, into = c("year", "month", "date"), sep = "-")

ui <- dashboardPage(
  dashboardHeader(title = "Rocket Statistics from 1957-2018"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      box(title = "Plot (RENAME PENDING)", width = 12,
          radioButtons("x", "Select Fill", choices = c("mission_status", "rocket_status", "country"),
                       selected = "mission_status"),
          plotOutput("plot")
      ),#closes first box
      box(title = "Rocket Pictures", width = 5,
          selectInput("image_select", "Select Image:", choices=c("Cosmos 2I", "Cosmos 3M","Molniya M","Soyuz U", "Voskhod")),
          imageOutput("image"),
      ),#closes second box
    )#closes fluid row
  )#closes dashboard body
  
) #closes dashboard page

server <- function(input, output, session) {
  output$image <- renderImage({
    list(src = paste0("images/", input$image_select, ".png"), contentType = "image/png", width = "300px")
  }) #closes render image
  output$plot <- renderPlot({
    spacemission_fixed %>%
      group_by(year) %>%
      ggplot(aes_string(x="year", fill = input$x))+
      geom_bar() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  }) #closes render plot
  session$onSessionEnded(stopApp)
} #closes session

shinyApp(ui, server)