#
# STEEM/SBD To Fiat Conversion Optimizer
#
# This app identifies the best way to move STEEM or SBD from steemit to
# a target fiat currency. Currently, only USD through GDAX is supported as
# a target.
#

library(shiny)
library(ggplot2)

source("download_currency_data.R")

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("STEEM/SBD To Fiat Conversion Optimizer"),
   
   # Sidebar with parameters to configure
   sidebarLayout(
      sidebarPanel(
        numericInput("input_amount", "Input Amount:", 20),
        selectInput("input_currency", "Input Currency:",
                    c("SBD" = "sbd",
                      "STEEM" = "steem")),
        selectInput("target_currency", "Target Currency:",
                    c("USD" = "usd")),
        selectInput("target_exchange", "Target Exchange:",
                    c("GDAX" = "gdax")),
        checkboxGroupInput("conversion_exchanges", "Conversion Exchanges:",
                           c("blocktrades" = "blocktrades"),
                           selected = "blocktrades"),
        actionButton("calculate", "Calculate", class = "btn-primary")
      ),
      
      mainPanel(
        h3(textOutput("price")),
        h3(textOutput("total")),
        plotOutput("priceChart")
      )
   )
)

# Define server logic required to show prices and transactions
server <- function(input, output) {
   
   output$priceChart <- renderPlot({
     if (input$input_currency == "steem"){
       plot_currency_price_week(get_chart_data("steem"), "STEEM")
     }
     else if(input$input_currency == "sbd"){
       plot_currency_price_week(get_chart_data("sbd"), "SBD")
     }
   })
   
   output$price <- renderText({ 
     if (input$input_currency == "sbd"){
       print_currency_price(get_usd_price("sbd"), "SBD")
     }
     else if (input$input_currency == "steem"){
       print_currency_price(get_usd_price("steem"), "STEEM")
     }
   })
   
   output$total <- renderText({ 
     if (input$input_currency == "sbd"){
       print_currency_total(get_usd_price("sbd"), input$input_amount, "SBD")
     }
     else if (input$input_currency == "steem"){
       print_currency_total(get_usd_price("steem"), input$input_amount, "STEEM")
     }
   })
}

# Utility function to make price chart
plot_currency_price_week <- function(data, currency){
  ggplot(data, aes(date, price)) + 
    scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") + 
    ggtitle(paste(currency, "Price Last 7 Days", sep = " ")) +
    labs(x = "Date", y = "Price ($)") +
    geom_line(color = "blue")
}

# Utility function to format price string
print_currency_price <- function(price, currency){
  paste0(currency, " Price: $", sprintf("%.2f", price))
}

# Utility function to format price string
print_currency_total <- function(price, amount, currency){
  total <- sprintf("%.2f", amount*price)
  paste0("USD Total: $", total, " (", amount, " @ $",sprintf("%.2f", price), ")")
}

# Run the application 
shinyApp(ui = ui, server = server)

