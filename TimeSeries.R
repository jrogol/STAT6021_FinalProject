
library(zoo)
library(dygraphs)
library(dplyr)
library(tidyr)


ts_data <- data %>%
  group_by(market, date) %>%
  summarise(total_spend=sum(new_spend)) %>%
  spread(market, total_spend, fill =0) %>%
  arrange(date)


z <- zooreg(ts_data,
            start = as.yearqtr("2006-1"),
            frequency = 4)

dygraph(z[,-1]) %>%
  dyLegend(showZeroValues = FALSE)

total <- data %>%
  group_by(date) %>%
  summarise(total = sum(new_spend)) %>%
  arrange(date)

total <- zooreg(total[,2], start = as.yearqtr("2006-1"),
         frequency = 4)

ts <- ts(total$total, start = c(2006,1), frequency = 4)

plot(ts)
plot(decompose(ts))
par(mfrow=c(1,2))
acf(ts)
pacf(ts)
plot(diff(ts, 4))

library(tseries)
library(forecast)
adf.test(diff(ts,4), alternative="stationary", k=0)

fit <- arima(ts, c(1, 1, 1),seasonal = list(order = c(1, 1, 1), period = 4))


pred <- predict(fit, n.ahead = 16)
ts.plot(ts, pred$pred)

fit2 <- auto.arima(ts)
fcast <- forecast(fit2,36)
plot(fcast)

hw <- HoltWinters(ts)
p <- predict(hw, n.ahead = 36, prediction.interval = TRUE)
all <- cbind(ts, p)

dygraph(all, "Total Spend in London") %>%
  dySeries("ts", label = "Actual") %>%
  dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")






library(shiny)


ui <- pageWithSidebar(
  headerPanel('London Tourism'),
  sidebarPanel(
    selectInput('method', 'Method', levels(data$method)),
    selectInput('purpose', 'Purpose', levels(data$purpose)),
    selectInput('duration','Length of Stay', levels(data$duration))
  ),
  mainPanel(
    dygraphOutput('plot1')
  )
)

server <- function(input, output, session) {
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    filter <- data[data$method == input$method &
           data$duration == input$duration &
           data$purpose == input$purpose,]
    
    selected <- filter %>%
      group_by(date) %>%
      summarise(total = sum(new_spend)) %>%
      arrange(date)
    
    
    ts_data <- selected %>%
      group_by(market, date) %>%
      summarise(total_spend=sum(new_spend)) %>%
      spread(market, total_spend, fill =0) %>%
      arrange(date)
    
    zooreg(ts_data,
                start = as.yearqtr("2006-1"),
                frequency = 4)
  })
  
  output$plot1 <- renderDygraph({
    dygraph(selectedData) %>%
      dyLegend(showZeroValues = FALSE)
    })
}

# Launch the Server
shinyApp(ui, server)


selected <- data[data$method == "Air" &
                   data$duration == "1-3  nights" &
                   data$purpose == "VFR",]

ts_data <- selected %>%
  group_by(market, date) %>%
  summarise(total_spend=sum(new_spend)) %>%
  spread(market, total_spend, fill =0) %>%
  arrange(date)

z <- zooreg(ts_data,
            start = as.yearqtr("2006-1"),
            frequency = 4)
z <- z[,-1]
dygraph(z[,c("Switzerland","USA")])
