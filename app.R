#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(data.table)
library(dplyr)
library(plotly)
library(shinyWidgets)

not_sel <- "Not Selected"
ui = fluidPage(
  setBackgroundColor("ghostwhite"),
  titlePanel("Data visualization"),
  
  
  navbarPage(
   
    "",
     tabPanel("Overview",
             sidebarPanel(
               fileInput("file1", "Choose CSV File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
              tags$p("Note: Input only the cleaned and tidy formart  csv file") 
             ),
             mainPanel(
               
               fluidRow(
                 
                 box(valueBoxOutput("Total"),width = 100),
                 box(valueBoxOutput("Variables"), width = 100),
                 box(valueBoxOutput("Missing"), width = 100)
               ),
               br(),
               DT::dataTableOutput("data")
                          
                 )
               
             
    ),
    tabPanel("One qualitative variable", 
             sidebarPanel(
               selectInput("num_var_1", "Categorical Variable", choices = c(not_sel)),
               br(),
               actionButton("run_button", "Run", icon = icon("play"))
               
             ), 
             mainPanel("Bar plot",
               plotOutput("plot1") 
             )
            
             ),
    tabPanel("two qualitative variables", 
             
             sidebarPanel(
               selectInput("fac_var_1", "Categorical variable 1", choices = c(not_sel)),
               selectInput("fac_var_2", "Categorical variable 2", choices = c(not_sel)),
               br(),
               actionButton("run_button1", "Run", icon = icon("play"))
               
             ), 
             mainPanel("Stacked bar plot",
               plotOutput("plot2") 
             )
             ),
    
    tabPanel("One quantitaive variable",
             
             
               sidebarPanel(
               selectInput("num_var_3", "Numerical Variable ", choices = c(not_sel)),
            
               br(),
               actionButton("run_button2", "Run", icon = icon("play"))
               
             ), 
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   title = "plot",
                   plotOutput("plot3")
                 ),
                 tabPanel(
                   title = "Summary statistics",
                   tableOutput("summary")
                  
                 )
               )
             )
         ),
    tabPanel("One quantitative and one qualitative variables", 
                    
                    sidebarPanel(
                      selectInput("num_var_4", "Numerical variable ", choices = c(not_sel)),
                      selectInput("fac_var_3", "Categorical variable ", choices = c(not_sel)),
                      br(),
                      actionButton("run_button3", "Run ", icon = icon("play"))
                      
                    ), 
                    mainPanel("side by side box plot",
                      plotOutput("plot4") 
                    )
         ),
    tabPanel("Two quantitative variables", 
             
             sidebarPanel(
               selectInput("num_var_5", "Numerical variable ", choices = c(not_sel)),
               selectInput("num_var_6", "Numerical variable ", choices = c(not_sel)),
               selectInput("fac_var", "group variable ", choices = c(not_sel)),
               br(),
               actionButton("run_button4", "Run ", icon = icon("play"))
               
             ), 
             mainPanel("scatter plot",
               plotOutput("plot5") 
             )
    )
    
    
             
    )
    
  )


draw_plot_1 <- function(data_input, num_var_1){
  ggplot(data = data_input,
         aes_string(x = num_var_1)) +
    geom_bar()
}

draw_plot_2 <- function(data_input, fac_var_1, fac_var_2){
  ggplot(data = data_input, aes_string(x = fac_var_1 ,  fill = fac_var_2)) + 
    geom_bar()

}

draw_plot_3 <- function(data_input, num_var_3){

  
  ggplot(data = data_input, aes_string(x = num_var_3  )) + 
    geom_histogram(fill = "#69b3a2")
  
}


draw_plot_4 <- function(data_input, num_var_4, fac_var_3){
  
  ggplot(data_input, aes_string(x=fac_var_3, y=num_var_4, fill=fac_var_3)) +
    geom_boxplot()
  
}


draw_plot_5 <- function(data_input, num_var_5, num_var_6, fac_var){
  ggplot(data = data_input,
         aes_string(x = num_var_5, y = num_var_6, color = fac_var)) +
    geom_point()
}
server = function(input, output, session) {
  
  
 
  
  output$Missing<- renderValueBox({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    total_missing_count = sum(is.na(df))
           valueBox("Missing values", total_missing_count, color = "red")
    
  })
  
  output$Total<- renderValueBox({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    total = nrow(df)
    valueBox("Total observation",
             total , color = "green")
  })
  
  output$Variables<- renderValueBox({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    
    variables = ncol(df)
    
    valueBox("Number of variable",
             variables,color = "orange")
  })
  
  
  output$data = DT::renderDataTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    df
  })
  
  
 
  options(shiny.maxRequestSize=10*1024^2) 
  
  data_input <- reactive({
    req(input$file1)
   df<- read.csv(input$file1$datapath)
   
  })
  
  #######plot qualitative
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    
  })
  
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  
  
  plot1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input(), num_var_1())
  })
  
  output$plot1 <- renderPlot(plot1())
  
  ###########plot two qualitative
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "fac_var_1", choices = choices)
    updateSelectInput(inputId = "fac_var_2", choices = choices)
  })
  
  fac_var_1 <- eventReactive(input$run_button1,input$fac_var_1)
  fac_var_2 <- eventReactive(input$run_button1,input$fac_var_2)

  plot2 <- eventReactive(input$run_button1,{
    draw_plot_2(data_input(), fac_var_1(), fac_var_2())
  })
  output$plot2 <- renderPlot(plot2())
 
  
  ########one quanti
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_3", choices = choices)
    
  })
  
  
  num_var_3 <- eventReactive(input$run_button2,input$num_var_3)
 
  
  plot3 <- eventReactive(input$run_button2,{
    draw_plot_3(data_input(), num_var_3())
    
  })
  output$plot3 <- renderPlot(plot3())
  
  
  
 s <- reactive({
   req(input$file1)
   df1<- read.csv(input$file1$datapath)
   df1 <- df1 %>% select(input$num_var_3) %>% summary()
   
 }) 
    
 output$summary <- renderTable(s())
 
  ####plot qual vs quan
  
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_4", choices = choices)
    updateSelectInput(inputId = "fac_var_3", choices = choices)
  })
  
  num_var_4 <- eventReactive(input$run_button3,input$num_var_4)
  fac_var_3 <- eventReactive(input$run_button3,input$fac_var_3)
  
  plot4 <- eventReactive(input$run_button3,{
    draw_plot_4(data_input(), num_var_4(), fac_var_3())
  })
  output$plot4 <- renderPlot(plot4())
  
  
  ###two quan
  
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_5", choices = choices)
    updateSelectInput(inputId = "num_var_6", choices = choices)
    updateSelectInput(inputId = "fac_var", choices = choices)
  })
  
  num_var_5 <- eventReactive(input$run_button4,input$num_var_5)
  num_var_6 <- eventReactive(input$run_button4,input$num_var_6)
  fac_var <- eventReactive(input$run_button4,input$fac_var)
  
  plot5 <- eventReactive(input$run_button4,{
    draw_plot_5(data_input(), num_var_5(), num_var_6(),fac_var())
  })
  output$plot5 <- renderPlot(plot5())
  
}



shinyApp(ui = ui, server = server)

