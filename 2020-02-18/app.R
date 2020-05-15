library(shiny)
library(tidyverse)
library(plotly)
theme_set(theme_light())

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Food Consumption & Co2 Emmission in 2018"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("food_category",
                  "Food category:",
                  choices = unique(food_consumption$food_category))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("food_country", height = "400px"),
      plotlyOutput("emission_country", height = "400px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  world <- map_data("world")
  food_consumption <- food_consumption %>% 
    mutate(Consumption = consumption,
           Co2 = co2_emmission,
           Country = country) %>% 
    select(-consumption, -co2_emmission, -country)
    
  output$food_country <- renderPlotly({
    p <- food_consumption %>% 
      left_join(world %>% mutate(Country = region), by = 'Country') %>% 
      filter(food_category == input$food_category) %>% 
      ggplot(aes(long, lat, group = group)) +
      geom_polygon(aes(fill = Consumption), color = "white")+
      scale_fill_viridis_c(option = "C") +
      labs(x = "", y = "", fill = "Consumption\n(kg/person/year)")
    ggplotly(p, tooltip = c("Country", "Consumption"))
    })
  
    output$emission_country <- renderPlotly({
      p <- food_consumption %>% 
        left_join(world %>% mutate(Country = region), by = 'Country') %>% 
        filter(food_category == input$food_category) %>% 
        ggplot(aes(long, lat, group = group)) +
        geom_polygon(aes(fill = Co2), color = "white")+
        scale_fill_viridis_c(option = "C") +
        labs(x = "", y = "", fill = "Co2 Emission\n(kg/person/year)")
      ggplotly(p, tooltip = c("Country","Consumption"))
      })
}

# Run the application 
shinyApp(ui = ui, server = server)