library(shiny)
library(tidyverse)

UAH <- read_delim("UAH-lower-troposphere-long.csv.bz2")

ui <- fluidPage(
  tabsetPanel(
    
    tabPanel("opening page",
             titlePanel("About"),
             HTML(paste("This data shows the <strong>temperature deviation</strong> from the 1991-2020 baseline. 
               Displayed below is 5 randomly selected rows of data.")),
             mainPanel(
               tableOutput("data")
             )
    ),
    
    tabPanel("plots",
             titlePanel("Plot of temperatures by regions"),
             
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("regions", "Select regions to display:", 
                                    choices = unique(UAH$region), 
                                    selected = unique(UAH$region))
               ),
               mainPanel(
                 plotOutput("plot")
               )
             )
    ),
    
    tabPanel("table",
             titlePanel("table of average temperature deviations by region"),
             
             sidebarLayout(
               sidebarPanel(
                 radioButtons("period", "Select Time Period:",
                              c("month", "year", "deacade"), selected = "Years")
               ),
               
               mainPanel(
                 tableOutput("table")
               )
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$data <- renderTable({
    UAH %>% 
      sample_n(5)
  })
  
  output$plot <- renderPlot({
    UAH %>% 
      filter(region %in% input$regions) %>% 
      group_by(region) %>% 
      ggplot(aes(x=year, y=temp, group = region, color=factor(region)))+
      geom_point()+
      labs(x="Year", y="Temperature deviation", color="Region")
  })
  
  
  output$table <- renderTable({
    
    if (input$period == "month") {
      filter_time <- UAH %>%
        group_by(year, month) %>%
        summarize(avg_temp = mean(temp))
    } else if (input$period == "year") {
      filter_time <- UAH %>%
        group_by(year) %>%
        summarize(avg_temp = mean(temp))
    } else {
      filter_time <- UAH %>%
        group_by(decade = 10*(year %/% 10)) %>%
        summarize(avg_temp = mean(temp))
    }
    
    filter_time
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

