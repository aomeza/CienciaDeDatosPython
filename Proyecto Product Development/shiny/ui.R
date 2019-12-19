
library(shiny)
library(readr)

#plumber_10cc1ae23137
#log <- read_log('../../logs/plumber_10cc1ae23137.log',
log <- read_log('../logs/plumber_10cc1ae23137.log', 
                col_names = c('Type','Timestamp','User','Endpoint','UserAgent','Date','Time','Model','Response'))



shinyUI(fluidPage(
    h3("Log Filter"),
    br(),
    sidebarPanel(
        selectInput("user", "User", choices = unique(log$User)),
        selectInput("model", "Model", choices = NULL),
        selectInput("response", "Response", choices = NULL)#,
        #tableOutput('data')
    ),
    
    mainPanel(
        tableOutput('data')
    )
    
    
))
