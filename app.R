library(shiny)
library(ggplot2)

# source toss() function
source('functions.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Saving and Investing Scenarios (workout02)"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,
           sliderInput("amount",
                       "Initial Amount",
                       min = 1,
                       max = 100000,
                       value = 1000)),
    column(4,
           sliderInput("rate",
                       "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5)),
   column(4,
          sliderInput("years",
                      "Years",
                      min = 0,
                      max = 50,
                      value = 10)),
   column(4,
          sliderInput("contrib",
                      "Annual Contribution",
                      min = 0,
                      max = 50000,
                      value = 2000)),
   column(4,
          sliderInput("growth",
                      "Growth Rate (in %)",
                      min = 0,
                      max = 20,
                      value = 2)),
   column(4,
          selectInput("facet",
                      "Facet?",
                      c("No", "Yes")))
  ),
  # Graph Plot
  h4("Timelines"),
  plotOutput("distPlot"),
  
  # Table
  h4("Balances"),
  tableOutput("balances")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot = renderPlot({
    
    no_contrib = c(rep(0, input$years + 1))
    fixed_contrib = c(rep(0, input$years + 1))
    growing_contrib = c(rep(0, input$years + 1))
    
    for(i in 0:(input$years + 1)) {
      no_contrib[i] = future_value(amount = input$amount, rate = input$rate/100, years = i - 1)
      fixed_contrib[i] = no_contrib[i] + annuity(contrib = input$contrib, rate = input$rate/100, years = i - 1)
      growing_contrib[i] = no_contrib[i] + growing_annuity(contrib = input$contrib, rate = input$rate/100, growth = input$growth/100, years = i - 1)
    }
    
    df = data.frame("Years" = c(0:input$years), "no_contrib" = no_contrib, "fixed_contrib" = fixed_contrib, "growing_contrib" = growing_contrib)
    
    if(input$facet == "No") {
      ggplot(data = df, aes(c(0:input$years))) +
        geom_line(aes(y = no_contrib, colour = "no_contrib")) +
        geom_line(aes(y = fixed_contrib, colour = "fixed_contrib")) +
        geom_line(aes(y = growing_contrib, colour = "growing_contrib"))
      #print(df)
    } else {
      ggplot(data = df, aes(x = years, y = values)) +
        geom_line() +
        facet_wrap(~investment_type)
    }
   
  })
  
  output$balances = renderTable({
    no_contrib = c(rep(0, input$years + 1))
    fixed_contrib = c(rep(0, input$years + 1))
    growing_contrib = c(rep(0, input$years + 1))
    
    for(i in 0:(input$years + 1)) {
      no_contrib[i] = future_value(amount = input$amount, rate = input$rate/100, years = i - 1)
      fixed_contrib[i] = no_contrib[i] + annuity(contrib = input$contrib, rate = input$rate/100, years = i - 1)
      growing_contrib[i] = no_contrib[i] + growing_annuity(contrib = input$contrib, rate = input$rate/100, growth = input$growth/100, years = i - 1)
    }
    
    df = data.frame("Years" = c(0:input$years), "no_contrib" = no_contrib, "fixed_contrib" = fixed_contrib, "growing_contrib" = growing_contrib)
    head(df, n = 11)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

