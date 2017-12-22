library(shiny)
library(shinydashboard)
library(shinythemes)
library(highcharter)
library(plotly)
library(qtlcharts)

ui =
  
  dashboardPage(skin = 'purple',
                
                dashboardHeader(title = "Statistics Dashboard"),
                
                dashboardSidebar(
                  sidebarMenu(
                   
                           fileInput('file', 'Choose CSV File',
                                     accept = c(
                                       'text/csv',
                                       'text/comma-separated-values',
                                       '.csv'
                                     )
                           ),
                           
                           actionButton("submit", "Load File"),
                           
                           selectInput("columns", "Select Column for Univariate Analysis", choices = NULL),
                           selectInput("columns2", "Select Column X (Bivariate Analysis):", choices = NULL),
                           selectInput("columns3", "Select Column Y (Bivariate Analysis):", choices = NULL)
                  )
                           
                           ),
                  
                  
                  
                
                dashboardBody(
                  
                    
                  fluidRow(
                    
                    valueBoxOutput("meanbox"),
                    
                    valueBoxOutput("medianbox"),
                    
                    valueBoxOutput("sdbox")),
                    
                  fluidRow(
                    box(title = "Density Plot", solidHeader = TRUE,
                        collapsible = TRUE, color="purple", highchartOutput("density")),
                    
                    box(title = "Boxplot",  solidHeader = TRUE,
                        collapsible = TRUE,color="purple", plotlyOutput("box"))),
                  
                  fluidRow(
                    box(title = "Correlation Chart",  solidHeader = TRUE, width = 12, align = "right",
                        collapsible = TRUE,color="purple", iplotCorr_output("corr"))),
                    
                    fluidRow(
                      box(title = "Regression Chart",  solidHeader = TRUE, width = 12, align = "right",
                          collapsible = TRUE,color="purple", plotlyOutput("reg"))),
                    
                  fluidRow(
                    box(title = "Table", solidHeader = TRUE, width = 12,
                        collapsible = TRUE, color="purple",div(style = 'overflow-x: scroll',DT::dataTableOutput("table1")))
                  )
                  )
                )
  

server <- function(input, output, session) { 
  
  
  
  mydata = eventReactive(input$submit, {
    
    data = input$file
    req(data)
    my_dataset = read.csv(data$datapath)
    nums = sapply(my_dataset, is.numeric)
    dd2 = my_dataset[,nums]
    vars = names(dd2)
    updateSelectInput(session,"columns","Select Columns", choices = vars)
    updateSelectInput(session,"columns2","Select Column X:", choices = vars)
    updateSelectInput(session,"columns3","Select Column Y:", choices = vars)
    
    my_dataset
  })
  
  
  
  output$meanbox = renderValueBox({
    
    dd = mydata()
    valueBox(round(mean(dd[,input$columns], na.rm = TRUE),2)
             , "Mean", icon = icon("signal"),
             color = "purple"
    )
  })
  
  
  output$medianbox = renderValueBox({
    
    dd = mydata()
    valueBox(round(median(dd[,input$columns], na.rm = TRUE),2)
             , "Median", icon = icon("signal"),
             color = "purple"
    )
  })
  
  
  output$sdbox = renderValueBox({
    
    dd = mydata()
    valueBox(round(sd(dd[,input$columns], na.rm = TRUE),2)
             , "Standard Deviation", icon = icon("signal"),
             color = "purple"
    )
  })
  
  
  output$table1 = DT::renderDataTable({
    
    mydata()
  })
  
  
  output$density =
    renderHighchart({
      
      dd = mydata()
      hchart(density(dd[,input$columns]), type = "area", color = "#4B0082", name = "Density")
      
    })
  
  output$box =  
    renderPlotly({
      
      dd = mydata()
      plot_ly(type = "box") %>%
      add_boxplot(y = dd[,input$columns], name = as.character(paste(input$columns)), color = I("purple"))
    })
  
  output$corr =  
    iplotCorr_render({
      dd = mydata()
      nums = sapply(dd, is.numeric)
      dd2 = dd[,nums]
      iplotCorr(dd2)
    })
  
  output$reg =  
    renderPlotly({
      
      dd = mydata()
      nums = sapply(dd, is.numeric)
      dd2 = dd[,nums]
      m <- lm(as.formula(paste(input$columns3, '~', input$columns2)), data = dd2)
      x <- list(
        title = as.character(paste(input$columns2))
      )
      y <- list(
        title = as.character(paste(input$columns3))
      )
      broom::augment(m) %>%
        plot_ly(x = ~dd2[,input$columns2], showlegend = FALSE) %>%
        layout(xaxis = x, yaxis = y) %>%
        add_markers(y = ~dd2[,input$columns3], color = I("purple")) %>%
        add_ribbons(ymin = ~.fitted - 1.96 * .se.fit, 
                    ymax = ~.fitted + 1.96 * .se.fit, color = I("gray80")) %>%
        add_lines(y = ~.fitted, color = I("black"))
      
    })
      
      

  
  
  
}

shinyApp(ui, server)