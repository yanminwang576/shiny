rm(list=ls())
source('data315.R')

index <- c('Total reported admissions to hospital and diagnoses in hospital' = 1,
           'Estimated new hospital cases' = 2,
           'Estimated new admissions to hospital from the community' = 3,
           'Estimated new hospital admissions from the community with 3-7 day lagging' = 4,
           'Total reported hospital admissions and diagnoses from a care home' = 5,
           'Total beds - occupied by confirmed COVID-19 patients' = 6,
           'Mechanical Ventilation beds - occupied by confirmed COVID-19 patients' = 7)

# Define UI for application that draws a histogram
mystats <- function(x, na.omit=FALSE){ 
  options(digits = 4)
  if (na.omit) 
    x <- x[!is.na(x)] 
  m <- mean(x);  min <- min(x);  q1  <- as.numeric(quantile(x,0.25))
  me <- median(x);  q3  <- as.numeric(quantile(x,0.75)) ;  max <- max(x)
  n <- as.integer(length(x) );  s <- sd(x) 
  return(c(N = n, Min = min,Mean = m, Q1 = q1, 
           Median = me, Q3 = q3,Max = max, Stdev = s)) 
} 

library(shiny)
ui <- fluidPage(
  titlePanel("Hospital Burden in England"),
  selectInput("index", "Index", choices = index, width = '60%'),
  selectInput("region", "Region", choices = unique(mydata$region), width = '25%'),

  navlistPanel(
    "Data",
    tabPanel("Orgininal Data",DT::dataTableOutput("mytable")),
    "Depicting",
    tabPanel("Descriptive Statistics",verbatimTextOutput("sum")),
    tabPanel("Line Graph",plotOutput('graph')),
    "Modelling",
    tabPanel("Model Summary",verbatimTextOutput('summary')),
    tabPanel("Model Evaluation",plotOutput("qqplot"),verbatimTextOutput("Boxtest")),
    "Forecast",
    tabPanel("Forecast Value",tableOutput('fore')),
    tabPanel("Forecast Graph",plotlyOutput("foregraph")),
    tabPanel("Forecast Accuracy",tableOutput("accuracy"))
  )
)

# Define server 
server <- function(input, output,session) {

   df <-  reactive({mydata %>% 
      filter(indicator == input$index, region == input$region) %>% 
      transmute(time = as.Date(time, origin = "1899-12-30"),
      value = as.numeric(value)) %>% select(time,value) })

  #add dynamic label for variable value.
  url <- 'Source:https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/'
  lab <- reactive({paste(names(index)[as.numeric(input$index)],'(',input$region,')',sep='')})
  output$sum <- renderPrint({ mystats(df()$value)  })
  output$mytable <- DT::renderDataTable({df()}) 
#text the summit of ICU
  output$graph <- renderPlot({if (input$index != 7) {ggplot(data = df())+ aes(x = time,y = value) + 
                                geom_line() + labs(title = lab(), y = lab(),caption = url)} else 
                                  {ggplot(data = df())+ aes(x = time,y = value) + 
                                      geom_line() + labs(title = lab(), y = lab(),caption = url) +
                                      ylim(0,6000)  } })
  
  #sample-in forecast and sample-out forecast
  split_ts <-reactive({ 
    split_ts <- ts_split(ts(df()$value), sample.out  = 15)
  })
  #fit

  fit_out <- reactive({auto.arima(ts(df()$value))})
  output$summary <- renderPrint(fit_out())
  output$Boxtest <- renderPrint(Box.test(fit_out()$residuals, type='Ljung-Box'))
  df_fit <- reactive({ data.frame(resid = as.numeric(fit()$residuals)) })

  #sample-in forecast
  fit_in <- reactive({auto.arima(split_ts()$train)})
  fc_in <- reactive({forecast(fit_in (), h = 15)})
  output$fore_in <- renderTable(fc_in())
  
  output$accuracy_in <- renderTable({accuracy(fc_in(),split_ts()$test)}, rownames = TRUE)
  
  output$foregraph_in <- renderPlotly(print(plot_ly() %>% add_trace(x = df()$time,y= as.numeric(df()$value),
                                   mode = "lines+markers", name = "Actual", type = "scatter", line = list(color = "#00526d"), 
                                   marker = list(color = "#00526d")) %>% add_trace(x = df()$time,y = c(fc()$fitted,                                                                                                       rep(NA,length(fc()$mean))), 
                                  mode = "lines+markers", name = "Fitted", type = "scatter", line = list(color = "red"), 
                                   marker = list(color = "red")) %>% add_trace(x = df()$time,y = c(rep(NA,length(fc()$fitted)),fc()$mean), 
                                   mode = "lines+markers", name = "Forecasted", type = "scatter",marker = list(color = "green"), 
                                   line = list(color = "green")) %>% layout(title = " Actual vs Forecasted and Fitted")))
  #sample-out forecast
  fc_out <- reactive({forecast(fit(), h = 15)})
  output$fore <- renderTable(fc_out())
  
  output$accuracy <- renderTable({accuracy(fc_out())}, rownames = TRUE)
  
  output$foregraph <- renderPlotly(print(plot_ly() %>% add_trace(x = df()$time,y= as.numeric(df()$value),
                    mode = "lines+markers", name = "Actual", type = "scatter", line = list(color = "#00526d"), 
                    marker = list(color = "#00526d")) %>% add_trace(x = df()$time,y = c(fc()$fitted,rep(NA,length(fc()$mean))), 
                    mode = "lines+markers", name = "Fitted", type = "scatter", line = list(color = "red"), 
                    marker = list(color = "red")) %>% add_trace(x = df()$time,y = c(rep(NA,length(fc()$fitted)),fc()$mean), 
                    mode = "lines+markers", name = "Forecasted", type = "scatter",marker = list(color = "green"), 
                    line = list(color = "green")) %>% layout(title = " Actual vs Forecasted and Fitted")))
}

# Run the application 
shinyApp(ui = ui, server = server)
