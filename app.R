library(tidyverse)
library(shiny)
library(shinydashboard)

spacemission_fixed <- read_csv("data/space_missions_fixed.csv") %>% 
  separate("Location", into = c("site1", "site2", "site3", "site4", "Area", "Country"), extra = "drop", fill = "left", sep = ",") %>%
  select(-site1, -site2, -site3, -site4) %>%
  separate(Date, into = c("Year", "Month", "Date"), sep = "-")

ui <- dashboardPage(
  dashboardHeader(title = "Space Missions"),
  dashboardSidebar(
    title = "Data Collected from:",
  text = "Maven Analytics"), 
  
  dashboardBody(
    
    fluidRow(
      box(title = "Space Missions Over Time", width = 12,
          radioButtons("x", "Select Fill", choices = c("MissionStatus", "RocketStatus", "Country"),
                       selected = "Country"),
          plotOutput("plot1")
)#closes first box
),#closes fluid row

    fluidRow(
      box(title = "Rocket Pictures",
          selectInput("image_select", "Select Image", choices = c("Cosmos-3M (11K65M)", "Voskhod","Falcon 9 Block 5","Cosmos-2I (63SM)", "Soyuz U")),
          imageOutput("image")
), #close the second box 
      box(plotOutput("plot2")
) #closes third box
), #closes fluid row

) #closes dashboard body
) #closes dashboard page

server <- function(input, output, session) {
  
  output$image <- renderImage({
    list(src = paste0("images/", input$image_select, ".png"), contentType = "image/png", width = "300px")
  }, 
  deleteFile = FALSE) #closes render image
  
  output$plot1 <- renderPlot({
    spacemission_fixed %>%
      group_by(Year) %>%
      ggplot(aes_string(x="Year", fill = input$x))+
      geom_bar() + 
      labs(y = "Total Missions") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  }) #closes render plot
  
  output$plot2 <- renderPlot({
    spacemission_fixed %>%
      select(Rocket, Year) %>%
      filter(Rocket == input$image_select) %>%
      group_by(Year) %>%
      mutate(Year = as.numeric(Year)) %>%
      count(Year) %>%
      ggplot(aes(x = Year, y = n))+
      geom_col(fill = "purple")+
      labs(title = "Frequency a Selected Rocket was Launched Per Year",
           y = "Frequency")
  }) #closes render plot
  
  session$onSessionEnded(stopApp)
} #closes session

shinyApp(ui, server)