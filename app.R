library(shiny)
library(shinydashboard, warn.conflicts = FALSE)
library(shinythemes)
library(shinyWidgets)
library(SPEI)
library(readxl)

# Setting working directory
#setwd("C:/Users/rejid4996/Desktop/My Projects/Theia Images")

# Define UI for data upload app ----
ui <- navbarPage(
    position = "fixed-top", # To keep navigation bar static
    theme = shinytheme("cosmo"), # Applying slate theme
    
    fluid = TRUE,
    responsive = NULL,
    
    # Dashboard Title
    "SPI Prediction",
    
    # Default Selected Tab ----
    tabPanel("App", icon = icon("fire",  lib = "glyphicon"),
             
             # Page Break
             br(),br(),br(),br(),
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
                 
                 # Sidebar panel for inputs 
                 sidebarPanel(
                     
                     # Input: Select a file ----
                     fileInput("file1", "Choose the data File",
                               multiple = FALSE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv", '.xlsx')),
                     
                     
                     # Radio buttons to classify the type of image
                     radioButtons(
                         "radio",
                         label = "Type",
                         choices = list("SPI" = 1, "SPEI" = 2
                         ),
                         selected = 1
                         
                     ),
                     
                     # Invoking an action button for search option
                     actionButton("search", label = "Process", class = "btn-secondary"),
                     
                     br(), br(), br(),
                     strong("Connect with me"), br(),
                     verbatimTextOutput("about"),
                     br(),
                     strong("For more videos"), br(),
                     verbatimTextOutput("videos")
                     
                
                     
                 ),
                 

                 
                 # Main panel for displaying outputs ----
                 mainPanel(tabsetPanel(
                     tabPanel("Input Data", br(), 
                              "The Standardized Precipitation Index (SPI) is the most commonly used indicator worldwide for
detecting and characterizing meteorological droughts.The SPI indicator, which was developed by
McKee et al. (1993), and described in detail by Edwards and McKee (1997), measures precipitation
anomalies at a given location, based on a comparison of observed total precipitation amounts for
an accumulation period of interest (e.g. 1, 3, 12, 48 months), with the long-term historic rainfall
record for that period. The historic record is fitted to a probability distribution (the gamma 
distribution), which is then transformed into a normal distribution such that the mean SPI value 
for that location and period is zero.",
                              
                              
                              br(),
                              dataTableOutput("inputTable")),
                     tabPanel("Predicted SPI", br(),
                              
                              plotOutput("plot1"), plotOutput("plot2"), plotOutput("plot3"), plotOutput("plot4")),
                     
                     tabPanel("Output table", br(),
                              downloadButton("downloadData", "Download"), br(),
                              dataTableOutput("outputTable"))
                 ),
                    
                     
                     # Output: Data file ----
                     #verbatimTextOutput("text"),
                     
                     #imageOutput("myImage",width = "250px", height = "250px",inline = TRUE)
                     
                     
                 )
             )
    ),
    
    # Navigation menu tab
    tabPanel("Help",icon = icon("question-circle")),
    
    # Navigation menu tab
    navbarMenu("Users", icon = icon("users"), tabPanel("Profile", icon = icon("user")),
               tabPanel("Privacy", icon = icon("shield")), tabPanel("About", icon = icon("cogs")))
    

    
    )



# Define server logic to read selected file ----
server <- function(input, output) {
    
    data <- reactive({
        inFile <- input$file1
        
        if(is.null(inFile))
            return(NULL)
        file.rename(inFile$datapath,
                    paste(inFile$datapath, ".xlsx", sep=""))
        dataFile <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
        return(dataFile)
    })
    
    output$inputTable <-  renderDataTable({
        df <- data()
        df
        
    })
    
    

    
    output$plot1 <- renderPlot({
        df <- data()
        spi3 <- spi(df$Prcp, 3)
        plot.spei(spi3)
        
    })
    
    output$plot2 <- renderPlot({
        df <- data()
        spi6 <- spi(df$Prcp, 6)
        plot.spei(spi6)
        
    })
    
    output$plot3 <- renderPlot({
        df <- data()
        spi9 <- spi(df$Prcp, 9)
        plot.spei(spi9)
        
    })
    
    output$plot4 <- renderPlot({
        df <- data()
        spi12 <- spi(df$Prcp, 12)
        plot.spei(spi12)
        
    })
   
    
    output$about <- renderText({
        info <- c("Please reach out to 
https://www.linkedin.com/in/deepak-john-reji/ 
for more queries")
        info
    })
    
    output$videos <- renderText({
        data <- c("For more contents subscribe 
to my Youtube Channel 
https://www.youtube.com/channel/UCgOwsx5injeaB_TKGsVD5GQ")
    })
    
    output$outputTable <-  renderDataTable({
        df <- data()
        
        spi3 <- spi(df$Prcp, 3)
        
        spi6 <- spi(df$Prcp, 6)
        
        spi9 <- spi(df$Prcp, 9)
        
        spi12 <- spi(df$Prcp, 12)
        
        df$spi3 <- spi3$fitted
        df$spi6 <- spi6$fitted
        df$spi9 <- spi9$fitted
        df$spi12 <- spi12$fitted
        
        final_data = data.frame(df$Year, df$Month, df$Prcp, df$spi3, df$spi6, df$spi9, df$spi12)
        
        names(final_data) <- c('Year', 'Month', 'Precipitation', 'SPI 3', 'SPI 6', 'SPI 9', 'SPI 12')
        
        final_data
        
    })
    
    output$resultTable <- reactive({
        df <- data()
        
        spi3 <- spi(df$Prcp, 3)
        
        spi6 <- spi(df$Prcp, 6)
        
        spi9 <- spi(df$Prcp, 9)
        
        spi12 <- spi(df$Prcp, 12)
        
        df$spi3 <- spi3$fitted
        df$spi6 <- spi6$fitted
        df$spi9 <- spi9$fitted
        df$spi12 <- spi12$fitted
        
        final_data = data.frame(df$Year, df$Month, df$Prcp, df$spi3, df$spi6, df$spi9, df$spi12)
        
        names(final_data) <- c('Year', 'Month', 'Precipitation', 'SPI 3', 'SPI 6', 'SPI 9', 'SPI 12')
        
        return(final_data)
    })
    
    output$downloadData <- downloadHandler(
        filename = function(){"spi_predicted.csv"},
        content = function(fname){write.csv(resultTable(),fname,row.names = FALSE)}) 
}





# Run the app ----
shinyApp(ui, server)