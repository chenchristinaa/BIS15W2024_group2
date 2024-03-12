# libraries

library(tidyverse)
library(shiny)
library(shinydashboard)

# reading and cleaning data

spacemission_fixed <- read_csv("data/space_missions_fixed.csv") %>% 
  separate("Location", into = c("site1", "site2", "site3", "site4", "Area", "Country"), extra = "drop", fill = "left", sep = ",") %>%
  select(-site1, -site2, -site3, -site4) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-")

# shiny app

ui <- dashboardPage(
  dashboardHeader(title = "Space Missions"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("rocket")),
      menuItem("Information", tabName = "information", icon = icon("pencil")),
      menuItem("Our Dataset", tabName = "data", icon = icon("list"))
) # closes sidebar menu
), #closes dashboard sidebar
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Launch Statistics Over Time", width = 12,
                    radioButtons("x", "Select Fill", choices = c("MissionStatus", "RocketStatus", "Country"),
                                 selected = "Country"),
                    plotOutput("plot1")
) # closes first box
), # closes fluid row
              
              fluidRow(
                box(title = "Investigation of Specific Rockets",
                    selectInput("image_select", "Select Image", choices = c("Cosmos-3M (11K65M)", "Voskhod","Falcon 9 Block 5","Cosmos-2I (63SM)", "Soyuz U")),
                    imageOutput("image")
), #close the second box 
                box(plotOutput("plot2")
) #closes third box
), # closes fluid row
), # closes tab item

      tabItem(tabName = "information",
              fluidRow(
                box(title = "Sources",
                  HTML("<p>Data collected from:<p>
                        <li><a href='https://mavenanalytics.io/data-playground?page=5&pageSize=5'>Maven Analytics</a></li>
                        <p>Images collected from:<p>
                        <li><a href='https://www.defencetalk.com/wp-content/uploads/2011/06/soyuz-u-rocket-cosmos.jpg'>Cosmos-2I (63SM)</a></li>
                        <li><a href='https://space.skyrocket.de/img_lau/kosmos-3m__abrixas__1.jpg'>Cosmos-3M (11K65M)</a></li>
                        <li><a href='https://www.spacelaunchschedule.com/wp-content/uploads/falcon2520925_image_20210107183433.jpeg'>Falcon 9 Block 5</a></li>
                        <li><a href='https://spaceflight101.com/progress-ms-05/photos-final-soyuz-u-takes-shape-for-liftoff-from-baikonur/'>Soyuz U</a></li>
                        <li><a href='https://vtwp-media.s3-accelerate.amazonaws.com/2022/09/vostok-spacecraft-scaled.jpg'>Voskhod</a></li>"
) # closes HTML
), # closes box

              box(title = "Cleaning the Data",
                  HTML("<p>Modifications were made to a duplicated version of the csv of the dataset. Modifications included:</p>
                        <ul>
                        <li>fixed spelling of `plateform` to `platform` (x1)</li>
                        <li>added `USA` to New Mexico observations (x3)</li>
                        <li>fixed special characters of `Alacantara, Maranhao, Brazil` (x3)</li>
                        <li>added `Kekaha, Hawaii, USA` to Pacific Missile Range Facility observation (x1)</li>
                        <li>added `Spain` to Gran Canaria observations (x2)</li>
                        <li>added `International Waters` to Pacific Ocean observations (x36)</li>
                        <li>added `International Waters` to Barents Sea observations (x3)</li>
                        <li>added `International Waters` to Yellow Sea observations (x3)</li>
                        </ul>"
) # closes HTML
), # closes box

                box(title = "About the Data",
                  HTML("<p>The dataset contains information about space missions from 1957 
                        to August 2022. The dataset includes information about the date, 
                        location, mission status, rocket status, and country of the mission.
                        About the variables:</p>
                        <li>Company: company responsible for the space mission</li>
                        <li>Area: area within the country where the rocket was launched</li>
                        <li>Country: country where the rocket was launched</li>
                        <li>Year: year that the rocket was launched</li>
                        <li>Month: month that the rocket was launched</li>
                        <li>Day: day of month that the rocket was launched</li>
                        <li>Time: time of day the rocket was launched (UTC)</li>
                        <li>Rocket: name of rocket used</li>
                        <li>Mission: name of space mission</li>
                        <li>RocketStatus: status of the rocket as of August 2022</li>
                        <li>Price: price of the rocket in millions of US dollars</li>
                        <li>MissionStatus: status of the mission</li>"
) # closes HTML
), # closes box
 
                box(title = "Soyuz U Anomaly Explained",
                  HTML("<p>Beginning in 1973, the Soyuz family of rockets was developed by the USSR. 
                        The Soyuz-U rockets hold the world record for highest launch rate in a year 
                        (1979) and the Soyuz family was a dominant force in the USSR space race program. 
                        The launches post 2000s were the Soyuz U/Fregat model where a third stage of the 
                        launching process was added. Production of the Soyuz U was retired in 2015 after 
                        a series of mechanical failures.<p>"
) # closes HTML
), # closes box

                box(title = "Credits",
                  HTML("<p>This Shiny app was created by Christina Chen and Samantha Swan for their BIS15L course. Course details include:<p>
                        <ul>
                        <li>Name of Course: Introduction to Data Science for Biologists</li>
                        <li>Course Code: BIS15L</li>
                        <li>Time: Winter Quarter 2024</li>
                        <li>Instructor: Joel Ledford</li>
                        <li>TA: Bryshal Moore</li>
                        <li>Grade They're Hoping to Get: A+</li>
                        </ul>"
  
) # closes HTML
) # closes box
), # closes fluid row
), # closes tab item

      tabItem(tabName = "data",
             fluidRow(
                  title = "CSV Data",
                   dataTableOutput("csv_table")
) # closes fluid row
) # closes tab item
) # closes tab items
) # closes dashboard body
) # closes dashboard page

server <- function(input, output, session) {
  
  output$image <- renderImage({
    list(src = paste0("images/", input$image_select, ".png"), contentType = "image/png", width = "500px")
  }, 
  deleteFile = FALSE
) #closes render image
  
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
      geom_col(fill = "darkslateblue")+
      geom_text(aes(label = n), hjust = 0.5, vjust = -.75, size = 4, angle = 0, color = "black") +
      labs(title = paste("Frequency", input$image_select, "was Launched Per Year"),
           y = "Frequency")
}) #closes render plot

  output$csv_table <- renderDataTable({
    spacemission_fixed
}) # closes render data table
  
  session$onSessionEnded(stopApp)
} #closes session

shinyApp(ui, server)