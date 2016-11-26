library(shiny)
library(gapminder)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Logged GDP"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("yearInput", "Year", min = 1952, max = 2007, value = c(1966, 1993)),
         selectInput("countryInput", "Choose a country:", choices = gapminder$country)
         ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("mainPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  reduced_df <- reactive({
    gapminder %>%
      mutate(gdpPercap = log10(gdpPercap)) %>%
      filter(country == input$countryInput, year >= input$yearInput[1] & year <= input$yearInput[2])
     
  })
  output$mainPlot <- renderPlot({
    ggplot(data = reduced_df(), 
           aes(gdpPercap, lifeExp, color = year)) + 
      geom_point(size=5) + ggtitle(input$countryInput) + geom_smooth()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

