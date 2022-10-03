library(shiny)
library(tidyverse)
library(plotly)
library(reshape2)
library(openxlsx)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(readr)
library(stats)
library(stringr)
library(utils)
library(rsconnect)
library(packrat)


ui <- fluidPage(
  
  titlePanel("Shawnee Oil Company LLC Plot Generator"),
  
  tabsetPanel(
    
    tabPanel("Oil Production", fluid = TRUE,
             # sidebarLayout(
             #   sidebarPanel(
             #     "Accepts .xlsx files",
             #     fileInput("upload", "File Upload", multiple=F, accept=c(".xlsx"), buttonLabel="Browse..."),
             #     # checkboxInput("smoothed", "Smoothed"),
             #     numericInput("stdevs", "Standard Deviations", value=1, min=1, max=10, width="60%"),
             #     actionButton("run", "Run")
             #   ),
             mainPanel(fluidRow(
               column(6,
                      br(),
                      "Accepts .xlsx files",
                      fileInput("upload", "File Upload", multiple=F, accept=c(".xlsx"), buttonLabel="Browse...")
               ),
               column(4,
                      br(),
                      br(),
                      # checkboxInput("smoothed", "Smoothed"),
                      numericInput("stdevs", "Standard Deviations", value=1, min=1, max=10, width="60%")
               ),
               column(2,
                      br(),
                      br(),
                      br(),
                      actionButton("run", "Run")
               )),
               fluidRow(
                 br(),
                 plotlyOutput("plot", width = "1200", height = "680"),
                 br(),
                 br()
               ))
             
    ),
    
    tabPanel("Expenses", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 fileInput("upload2", "File Upload", multiple=F, accept=".xlsx", buttonLabel="Browse..."),
                 NULL #placeholder for more inputs
               ),
               mainPanel(fluidRow(
                 plotlyOutput("plot2")  
               ))
             )
    )
  )
)


server <- function(input, output, session) {
  
  RV <- reactiveValues()
  
  observeEvent(input$upload, {
    inFile <- input$upload
    RV$dataReceived <- read.xlsx(inFile$datapath, 1)
    
    #collapse data frame for plotting
    RV$dataMelt <- melt(RV$dataReceived)
    #get well names, rename rows as well names
    wellNames <- RV$dataReceived[,1]
    RV$dataReceived <- RV$dataReceived[,-1]
    rownames(RV$dataReceived) <- wellNames
    RV$numWells <- ncol(RV$dataReceived)
    #rename columns
    RV$dataMelt <- RV$dataMelt %>%
      rename(Well_Name = Name,
             Month = variable,
             Oil_Prod = value)
    #remove "Month.", change to numeric
    RV$dataMelt$Month <- gsub('Month.', '', RV$dataMelt$Month)
    RV$dataMelt$Month <- as.numeric(RV$dataMelt$Month)
    ##calculations
    #get averages
    meansList <- c()
    for(i in 1:RV$numWells){
      meansList[i] <- mean(as.numeric(RV$dataReceived[,i]), na.rm = TRUE)
    }
    #add averages columm to ataAvg data frame
    RV$dataAvg <- rbind(1:RV$numWells, meansList)
    RV$dataAvg <- as.data.frame(t(RV$dataAvg))
    colnames(RV$dataAvg) <- c("Month", "Average")
  })
  
  observeEvent(input$run, {
    #add standard deviations to dataAvg data frame
    RV$sdList <- c()
    for(i in 1:RV$numWells){
      RV$sdList[i] <- sd(RV$dataReceived[,i], na.rm = TRUE)
    }
    RV$dataAvg$sd = RV$sdList
    #compute upper and lower bounds
    for(i in 1:RV$numWells){
      RV$dataAvg$UpperBound[i] <- RV$dataAvg[i,2] + (RV$dataAvg[i,3] * input$stdevs)
      RV$dataAvg$LowerBound[i] <- RV$dataAvg[i,2] - (RV$dataAvg[i,3] * input$stdevs)
    }
    
    # ##SMOOTHING
    # if(input$smoothed){
    #   #loess smoothing for plot
    #   RV$dataSmoothed <- data.frame("Month" = RV$dataAvg[,1],
    #                              "Average" = predict(loess(RV$dataAvg[,2] ~ RV$dataAvg[,1])),
    #                              "Lower Bound" = predict(loess(RV$dataAvg[,5] ~ RV$dataAvg[,1])),
    #                              "Upper Bound" = predict(loess(RV$dataAvg[,4] ~ RV$dataAvg[,1])))
    #   RV$dataAvg <- RV$dataSmoothed
    # }
    
    #rounding
    RV$dataAvg <- round(RV$dataAvg, digits = 2)
    p <- plot_ly(data = dataMelt, 
                    x = ~Month, 
                    y = ~Oil_Prod,
                    type = 'scatter',
                    group = ~Well_Name,
                    mode = 'markers + lines',
                    text = paste(dataMelt$Well_Name,
                                 "<br>Month ", dataMelt$Month,
                                 "<br>Oil: ", dataMelt$Oil_Prod),
                    hoverinfo = 'text',
                    name = dataMelt$Well_Name) %>%
      layout(title = 'Cumulative Oil Production', xaxis = list(title = "Number of Months"), 
             yaxis = list(title = "Cum Oil (bbl)"))
    
    p <- add_trace(p = p,
                      x = RV$dataAvg[,1], 
                      y = RV$dataAvg[,4], 
                      type = 'scatter', 
                      mode = 'lines', 
                      text = paste("<br>Month ", RV$dataAvg[,1],
                                   "<br>y = ", RV$dataAvg[,4]),
                      hoverinfo = 'text',
                      name = "Upper Bound",
                      line = list(color = 'darkgrey', width = 1),
                      legendgroup = "avg") %>%
      #add lower bound and shading
      add_trace(x = RV$dataAvg[,1], 
                y = RV$dataAvg[,5], 
                type = 'scatter', 
                mode = 'lines', 
                text = paste("<br>Month ", RV$dataAvg[,1],
                             "<br>y = ", RV$dataAvg[,5]),
                hoverinfo = 'text',
                name = "Lower Bound",
                fill = 'tonexty', 
                fillcolor="rgba(0,40,100,0.2)",
                line = list(color = 'darkgrey', width = 1),
                legendgroup = "avg") %>%
      #add upper bound
      add_trace(x = RV$dataAvg[,1], 
                y = RV$dataAvg[,2], 
                type = 'scatter', 
                mode = 'lines', 
                text = paste("<br>Average",
                             "<br>Month ", RV$dataAvg[,1],
                             "<br>Oil: ", RV$dataAvg[,2]),
                hoverinfo = 'text',
                name = "Average",
                line = list(color = 'black', width = 4),
                legendgroup = "avg")
    
    output$plot = renderPlotly({
      p
    })
  })
}

shinyApp(ui = ui, server = server)

