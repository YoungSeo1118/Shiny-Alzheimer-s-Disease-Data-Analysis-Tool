#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("/Users/macintosh/Desktop/R_App/Data_Analysis_App/")


library(shiny)
source("lesson_7.R")

Columns<-c(colnames(demographic),colnames(abeta),colnames(tau),
           colnames(FDG),colnames(MRI),colnames(cognitive)) %>% unique()
not_sel<-"Not Selected"

# Define server logic required to draw a histogram
shinyServer(function(input,output){
    independent<-eventReactive(input$run_button,input$independent)
    dependent<-eventReactive(input$run_button,input$dependent)
    
    covariate<-reactive({
        switch(input$covariate,
               "NULL" = NULL,
               "DX" = list("DX"),
               "APOE4" = list("APOE4"),
               "DX, APOE4" = list("DX","APOE4"))
    })
    
    # covariate<-eventReactive(input$run_button,covariate())
    
    table<-eventReactive(input$run_button,{
        htmlTable(wrapper_function(independent(),dependent(),covariate())[[1]])
    })
    output$table<-renderUI(HTML(table()))
    
    plot<-eventReactive(input$run_button,{
        wrapper_function(independent(),dependent(),covariate())[[2]]
    })
    output$plot<-renderPlot(plot())
})
            
