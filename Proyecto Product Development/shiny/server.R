
library(shiny)
library(readr)
library(dplyr)

log <- read_log('../logs/plumber_10cc1ae23137.log', 
                col_names = c('Type','Timestamp','User','Endpoint','UserAgent','Date','Time','Model','Response'))

log$Date <- log$Date
log$Timestamp <- log$Timestamp
log$Time <- log$Time

shinyServer(function(input, output, session) {

    user <- reactive({
        log %>%
            filter(User == input$user)
    })
    
    observeEvent(user(), {
        choices <- unique(user()$Model)
        updateSelectInput(session, 'model', choices = choices)
        
    })
    
    
    model <- reactive({
        user() %>%
            filter(Model == input$model)
    })
    
    observeEvent(model(), {
        choices <- unique(model()$Response)
        updateSelectInput(session, 'response', choices = choices)
    })
    
    output$data <- renderTable({
        model() %>%
            filter(Response == input$response)
    })
})
