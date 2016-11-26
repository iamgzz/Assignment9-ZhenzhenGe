library(shiny)
library(babynames)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1880, max = 2014, value = c(1900, 2000)),
      textInput("nameInput", "Name",value="Kelly"),
      radioButtons("sexID", "Sex",  choices = c("Female only", "Male only", "Both"), selected = "Both")),
    mainPanel(
      plotOutput("mainplot"),
      tableOutput("results")
    )
  ),
  titlePanel("Baby Names")
)
server <- function(input, output, session) {
  reduced_df <- reactive({
    sex_vec <- switch(input$sexID,
                      `Female only` = "F",
                      `Male only` = "M",
                      Both = c("F", "M")
    )
    filter(
      babynames, 
      name == input$nameInput, 
      year >= input$yearInput[1] & year <= input$yearInput[2], 
      sex %in% sex_vec 
    )
  })
  output$mainplot <- renderPlot({
    ggplot(data = reduced_df(), 
           aes(year, n, colour = sex)) + 
      geom_line() + ggtitle(input$nameInput) + scale_color_manual(values=c("M" = "red", "F" = "green"))
  })
  output$results <- renderTable({ 
    reduced_df() %>%
      dplyr::select(-prop) %>%
      mutate(year = as.integer(year)) %>%
      spread(key = sex, value = n, fill = "0")
  })
}
shinyApp(ui = ui, server = server)