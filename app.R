#title:Stock Performance Overview for TDC
#author: Elida Vaccea

library(shiny)
library(plotly)
library(TTR)
library(forecast)
library(dplyr)

datas <- read.csv2("TDC_Stock.csv", header=TRUE, sep=";", dec =",", na.strings="NA")
datas$date <- as.Date(datas$date,"%d.%m.%Y")
datas <- arrange(datas,date)

# UI
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Stock TDC"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("numperrueck", "Number of Periods for Moving Average",
                     value = 10, min = 2, max = 20, step = 1),
        numericInput("numper", "Number of Prediction Periods",
                     value = 30, min = 0, max = 100, step = 1),
        numericInput("p", "Number of autoregressive terms",
                     value = 0, min = 0, max = 10, step = 1),
        numericInput("d", "Number of nonseasonal differences needed for stationarity",
                     value = 0, min = 0, max = 10, step = 1),
        numericInput("q", "Number of lagged forecast errors in the prediction equation",
                     value = 1, min = 0, max = 10, step = 1),
        dateInput("date1", label = h3("Start Date"), value = "2015-10-06"),
        dateInput("date2", label = h3("End Date"), value = "2017-10-06")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(type = "tabs", 
                     tabPanel("Plot", plotlyOutput("plot")), 
                     tabPanel("Table", tableOutput("table"))
                    )
      )
  )
))


# Server logic 
server <- shinyServer(function(input, output) {
  #source function
  source("forecast.R")
  
  #result from arima model
  resar <- reactive({
    resultarima <- forecastArima(datas, n.ahead = input$numper, p = input$p, d = input$d, q = input$q)
    resultarima$date <- as.Date(resultarima$date,"%d.%m.%Y")
    return(resultarima)
  })
  #Filter the data frame according to the selected dates
  filtered <- reactive ({
    d1 <- as.Date(input$date1)
    d2 <- as.Date(input$date2)
    re <- filter(resar(), between(date, d1, d2))
    return(re)
  })
  
 
  
  output$plot <- renderPlotly({
    filtered <- filtered()
    #get the row index for the start date
    #i1 <- which(filtered$date == input$date1)

    #get the value of close price for the start date
    #st1 <- filtered[i1,1]
   
    #insert column change for the calculation of performance
    # filtered[,"change"] <- NA
    
    #calculate the performance values and insert them in the change column
    #for (i in 1:nrow(filtered)){
    #  filtered[i,"change"] <- (as.numeric(filtered[i,"actual"])/st1 - 1)*100
    # }
    
    #get the row number 
    for (i in 1:nrow(filtered)){
      if (is.na(filtered[i,"actual"])){
      r <- i - 1
      break
      }
    }
  
    #insert column for Moving Average
    filtered[,"sma"] <- c(SMA(filtered[1:r,1], input$numperrueck),rep(NA, input$numper ))
   
    #moving sd on MA
    #runSD vom TTR ist auch gut
    #filtered[,"sd"] <- rollapply(filtered[,"sma"], width = input$numperrueck, FUN = sd, fill = NA)
  
    #moving sd on actual
    #filtered[,"sdactual"] <- rollapply(filtered[,"actual"], width = input$numperrueck, FUN = sd, fill = NA)
   

    
    plot_ly(filtered, mode = "lines", x = filtered[,6],colors =c('#8dd3c7','#1f78b4','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5')) %>%
      add_lines(y=filtered[,1], name ="Closing Price", color = "Actual") %>%
      add_lines(y=filtered[,2], name ="Trend", color = "Trend") %>%
      add_lines(y=filtered[,3], name ="Prediction", color = "Prediction") %>%
      add_lines(y=filtered[,4], name ="lower bound", color = "Lower bound") %>%
      add_lines(y=filtered[,5], name ="upper bound", color = "Upper bound") %>%
      add_lines(y=filtered[,7], name ="MA", color = "Moving Average") %>%
      layout(
        title = "ARIMA on TDC Stock overview 2015/10/06 - 2016/10/06",
        xaxis = list(
          rangeselector = list(
            y = 0,
            buttons = list(
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"),
              list(step = "all"))),
          rangeslider = list(type = "date")),
        yaxis = list(title = "Performance (%)"))
  })
  
  
  # Table Slide
  output$table <- renderTable({
    data.frame(filtered())
  })
}
)

# Run the application 
shinyApp(ui = ui, server = server)

