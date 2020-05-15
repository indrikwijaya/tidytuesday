library(shiny)
library(tidyverse)
library(plotly)
theme_set(theme_light())

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Food Consumption by Country in 2018"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("food_category",
                  "Food category:",
                  choices = unique(food_consumption$food_category))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("food_country", height = "700px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  world <- map_data("world")
  output$food_country <- renderPlotly({
    p <- food_consumption %>% 
      left_join(world %>% mutate(country = region), by = 'country') %>% 
      filter(food_category == input$food_category) %>% 
      ggplot() +
      geom_map(map = world,
               aes(long, lat, map_id = country, fill = consumption),
               color = "white", alpha = 0.2)
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)